       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSRT10.
      *************************************************************
      **   簡易ソート処理      DKY=NO_                           **
      **    #SORT起動 (CBLRUN)                                   **
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  STN-NO.
           03  STN-NO-1       PIC  X(003) VALUE SPACE.
           03  STN-NO-2       PIC  9(003) VALUE ZERO.
       01  W-DATA.
           02  HIZUKE.
             03  YY           PIC  9(002) VALUE ZERO.
             03  MM           PIC  9(002) VALUE ZERO.
             03  DD           PIC  9(002) VALUE ZERO.
           02  K-MSG          PIC  X(040) VALUE SPACE.
           02  W-KBN          PIC  9(002) VALUE ZERO.
           02  W-FID1.
             03  W-FID11      PIC  X(006) VALUE SPACE.
             03  W-FID12      PIC  9(003).
             03  W-FID13      PIC  X(003) VALUE SPACE.
           02  W-FID2.
             03  W-FID21      PIC  X(006) VALUE SPACE.
             03  W-FID22      PIC  9(003).
             03  W-FID23      PIC  X(003) VALUE SPACE.
           02  I-ID           PIC  X(012) VALUE SPACE.
           02  O-ID           PIC  X(012) VALUE SPACE.
           02  SRTKEY         PIC  X(062) VALUE SPACE.
           02  SRTOUT1        PIC  X(060) VALUE SPACE.
           02  SRTOUT2        PIC  X(060) VALUE SPACE.
           02  SRTSUM1        PIC  X(060) VALUE SPACE.
           02  SRTSUM2        PIC  X(049) VALUE SPACE.
           02  SRTSEL1        PIC  X(060) VALUE SPACE.
           02  SRTSEL2        PIC  X(038) VALUE SPACE.
      *
       01  ASORT-PAR.
           03  F              PIC  X(006) VALUE "#SORT;".
           03  F              PIC  X(017) VALUE
                "SRT=_IDE=MSD_IFI=".
           03  A-I-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(013) VALUE "_ODE=MSD_OFI=".
           03  A-O-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(042) VALUE
                "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
           03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
           03  A-SRTKEY       PIC  X(056) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_OUT=".
           03  A-SRTOUT1      PIC  X(060) VALUE SPACE.
           03  A-SRTOUT2      PIC  X(060) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_SUM=".
           03  A-SRTSUM1      PIC  X(060) VALUE SPACE.
           03  A-SRTSUM2      PIC  X(049) VALUE SPACE.
           03  F              PIC  X(010) VALUE "_TOT=_SEL=".
           03  A-SRTSEL1      PIC  X(060) VALUE SPACE.
           03  A-SRTSEL2      PIC  X(024) VALUE SPACE.
           03  F              PIC  X(006) VALUE "_ALT=_".
           03  F              PIC  X(002) VALUE "/>".
       01  BSORT-PAR.
           03  F              PIC  X(006) VALUE "#SORT;".
           03  F              PIC  X(017) VALUE
               "SRT=_IDE=MSD_IFI=".
           03  B-I-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(013) VALUE "_ODE=MSD_OFI=".
           03  B-O-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(034) VALUE
               "_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
           03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
           03  B-SRTKEY       PIC  X(062) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_OUT=".
           03  B-SRTOUT1      PIC  X(060) VALUE SPACE.
           03  B-SRTOUT2      PIC  X(060) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_SUM=".
           03  B-SRTSUM1      PIC  X(060) VALUE SPACE.
           03  B-SRTSUM2      PIC  X(049) VALUE SPACE.
           03  F              PIC  X(010) VALUE "_TOT=_SEL=".
           03  B-SRTSEL1      PIC  X(060) VALUE SPACE.
           03  B-SRTSEL2      PIC  X(026) VALUE SPACE.
           03  F              PIC  X(006) VALUE "_ALT=_".
           03  F              PIC  X(002) VALUE "/>".
       01  CSORT-PAR.
           03  F              PIC  X(006) VALUE "#SORT;".
           03  F              PIC  X(017) VALUE
                "SRT=_IDE=MSD_IFI=".
           03  C-I-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_ODE=".
           03  C-SRTODE       PIC  X(003) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_OFI=".
           03  C-O-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(042) VALUE
                "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
           03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
           03  C-SRTKEY       PIC  X(050) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_OUT=".
           03  C-SRTOUT1      PIC  X(060) VALUE SPACE.
           03  C-SRTOUT2      PIC  X(060) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_SUM=".
           03  C-SRTSUM1      PIC  X(060) VALUE SPACE.
           03  C-SRTSUM2      PIC  X(045) VALUE SPACE.
           03  F              PIC  X(010) VALUE "_TOT=_SEL=".
           03  C-SRTSEL1      PIC  X(060) VALUE SPACE.
           03  C-SRTSEL2      PIC  X(034) VALUE SPACE.
           03  F              PIC  X(006) VALUE "_ALT=_".
           03  F              PIC  X(002) VALUE "/>".
       01  DSORT-PAR.
           03  F              PIC  X(006) VALUE "#SORT;".
           03  F              PIC  X(009) VALUE "SRT=_IDE=".
           03  D-SRTIDE       PIC  X(003) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_IFI=".
           03  D-I-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(013) VALUE "_ODE=MSD_OFI=".
           03  D-O-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(043) VALUE
                "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=1_".
           03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
           03  D-SRTKEY       PIC  X(050) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_OUT=".
           03  D-SRTOUT1      PIC  X(060) VALUE SPACE.
           03  D-SRTOUT2      PIC  X(060) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_SUM=".
           03  D-SRTSUM1      PIC  X(060) VALUE SPACE.
           03  D-SRTSUM2      PIC  X(045) VALUE SPACE.
           03  F              PIC  X(010) VALUE "_TOT=_SEL=".
           03  D-SRTSEL1      PIC  X(060) VALUE SPACE.
           03  D-SRTSEL2      PIC  X(033) VALUE SPACE.
           03  F              PIC  X(006) VALUE "_ALT=_".
           03  F              PIC  X(002) VALUE "/>".
       01  ESORT-PAR.
           03  F              PIC  X(006) VALUE "#SORT;".
           03  F              PIC  X(017) VALUE
                "SRT=_IDE=MSD_IFI=".
           03  E-I-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_ODE=".
           03  E-SRTODE       PIC  X(003) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_OFI=".
           03  E-O-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(042) VALUE
                "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
           03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
           03  E-SRTKEY       PIC  X(056) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_OUT=".
           03  E-SRTOUT1      PIC  X(060) VALUE SPACE.
           03  E-SRTOUT2      PIC  X(060) VALUE SPACE.
           03  F              PIC  X(005) VALUE "_SUM=".
           03  E-SRTSUM1      PIC  X(060) VALUE SPACE.
           03  E-SRTSUM2      PIC  X(045) VALUE SPACE.
           03  F              PIC  X(010) VALUE "_TOT=_SEL=".
           03  E-SRTSEL1      PIC  X(060) VALUE SPACE.
           03  E-SRTSEL2      PIC  X(028) VALUE SPACE.
           03  F              PIC  X(006) VALUE "_ALT=_".
           03  F              PIC  X(002) VALUE "/>".
       01  PAR-SIZ.
           03  PAR-SIZ01      PIC  9(004) VALUE 512.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-MID1.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
           02  FILLER.
             03  0101C-MID1  PIC  X(040) .
             03  FILLER      PIC  X(011) VALUE "DATE.  /  /".
             03  0301C-MID1  PIC  9(002) .
             03  0401C-MID1  PIC  9(002) .
             03  0501C-MID1  PIC  9(002) .
       01  C-MID2.
           03  FILLER   PIC  X(025)  VALUE "*************************".
           03  FILLER   PIC  X(025)  VALUE "**                     **".
           03  FILLER   PIC  X(018)  VALUE   "  ソート処理実行中".
           03  FILLER   PIC  X(025)  VALUE "*************************".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
       PROCEDURE                   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-MID1
       CALL "SD_Init" USING
            "C-MID1" "X" "0" "0" "69" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "00C-MID1" "X" "0" "0" "12" " " "C-MID1"  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID1" " " "1" "0" "57" "00C-MID1" " "  RETURNING RESU.
       CALL "SD_Init" USING
          "0101C-MID1" "RX" "1" "20" "40" " " "01C-MID1" RETURNING RESU.
       CALL "SD_From" USING
            "0101C-MID1" BY REFERENCE K-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
         "0201C-MID1" "X" "1" "68" "11" "0101C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
          "0301C-MID1" "9" "1" "73" "2" "0201C-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
            "0301C-MID1" BY REFERENCE YY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
          "0401C-MID1" "9" "1" "76" "2" "0301C-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
            "0401C-MID1" BY REFERENCE MM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
          "0501C-MID1" "9" "1" "79" "2" "0401C-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
            "0501C-MID1" BY REFERENCE DD "2" "0" RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING
            "C-MID2" " " "0" "0" "93" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID2" "X" "12" "25" "25" " " "C-MID2"  RETURNING RESU.
       CALL "SD_Init" USING
          "02C-MID2" "X" "13" "25" "25" "01C-MID2" " "  RETURNING RESU.
       CALL "SD_Init" USING
          "03C-MID2" "X" "13" "27" "18" "02C-MID2" " "  RETURNING RESU.
       CALL "SD_Init" USING
          "04C-MID2" "X" "14" "25" "25" "03C-MID2" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "10" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO-2 TO W-FID12 W-FID22.
           ACCEPT HIZUKE FROM DATE.
      *
           ACCEPT W-KBN FROM ARGUMENT-VALUE.
           IF  W-KBN = 30
               ACCEPT D-SRTIDE FROM ARGUMENT-VALUE
           END-IF
           IF  W-KBN = 10 OR 11 OR 30 OR 13 OR 55
               ACCEPT I-ID FROM ARGUMENT-VALUE
           END-IF
           IF  W-KBN NOT = 11 AND 13 AND 55
               ACCEPT W-FID11 FROM ARGUMENT-VALUE
           END-IF
           IF  W-KBN = 00 OR 22 OR 01 OR 03
               MOVE W-FID1 TO I-ID
           END-IF
      *
           IF  W-KBN = 01 OR 55
               ACCEPT O-ID FROM ARGUMENT-VALUE
           END-IF
           IF  W-KBN = 03
               ACCEPT C-SRTODE FROM ARGUMENT-VALUE
               ACCEPT O-ID FROM ARGUMENT-VALUE
           END-IF
           IF  W-KBN = 13
               ACCEPT E-SRTODE FROM ARGUMENT-VALUE
               ACCEPT O-ID FROM ARGUMENT-VALUE
           END-IF
           IF  W-KBN = 22
               ACCEPT W-FID21 FROM ARGUMENT-VALUE
               MOVE W-FID2 TO O-ID
           END-IF
           IF  W-KBN = 00 OR 10 OR 30
               MOVE W-FID1 TO O-ID
           END-IF
           IF  W-KBN = 11
               MOVE I-ID TO O-ID
           END-IF
      *
           IF  SPACE = I-ID OR O-ID
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           ACCEPT SRTKEY FROM ARGUMENT-VALUE.
           ACCEPT SRTOUT1 FROM ARGUMENT-VALUE.
           ACCEPT SRTOUT2 FROM ARGUMENT-VALUE.
           ACCEPT SRTSUM1 FROM ARGUMENT-VALUE.
           ACCEPT SRTSUM2 FROM ARGUMENT-VALUE.
           ACCEPT SRTSEL1 FROM ARGUMENT-VALUE.
           ACCEPT SRTSEL2 FROM ARGUMENT-VALUE.
           ACCEPT K-MSG FROM ARGUMENT-VALUE.
      *
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
      *
           IF  W-KBN = 10 OR 01 OR 55
               MOVE I-ID TO A-I-ID
               MOVE O-ID TO A-O-ID
               MOVE SRTKEY TO A-SRTKEY
               MOVE SRTOUT1 TO A-SRTOUT1
               MOVE SRTOUT2 TO A-SRTOUT2
               MOVE SRTSUM1 TO A-SRTSUM1
               MOVE SRTSUM2 TO A-SRTSUM2
               MOVE SRTSEL1 TO A-SRTSEL1
               MOVE SRTSEL2 TO A-SRTSEL2
               CALL "CBLRUN" USING ASORT-PAR PAR-SIZ
           END-IF
           IF  W-KBN = 00 OR 11 OR 22
               MOVE I-ID TO B-I-ID
               MOVE O-ID TO B-O-ID
               MOVE SRTKEY TO B-SRTKEY
               MOVE SRTOUT1 TO B-SRTOUT1
               MOVE SRTOUT2 TO B-SRTOUT2
               MOVE SRTSUM1 TO B-SRTSUM1
               MOVE SRTSUM2 TO B-SRTSUM2
               MOVE SRTSEL1 TO B-SRTSEL1
               MOVE SRTSEL2 TO B-SRTSEL2
               CALL "CBLRUN" USING BSORT-PAR PAR-SIZ
           END-IF
           IF  W-KBN = 03
               MOVE I-ID TO C-I-ID
               MOVE O-ID TO C-O-ID
               MOVE SRTKEY TO C-SRTKEY
               MOVE SRTOUT1 TO C-SRTOUT1
               MOVE SRTOUT2 TO C-SRTOUT2
               MOVE SRTSUM1 TO C-SRTSUM1
               MOVE SRTSUM2 TO C-SRTSUM2
               MOVE SRTSEL1 TO C-SRTSEL1
               MOVE SRTSEL2 TO C-SRTSEL2
               CALL "CBLRUN" USING CSORT-PAR PAR-SIZ
           END-IF
           IF  W-KBN = 30
               MOVE I-ID TO D-I-ID
               MOVE O-ID TO D-O-ID
               MOVE SRTKEY TO D-SRTKEY
               MOVE SRTOUT1 TO D-SRTOUT1
               MOVE SRTOUT2 TO D-SRTOUT2
               MOVE SRTSUM1 TO D-SRTSUM1
               MOVE SRTSUM2 TO D-SRTSUM2
               MOVE SRTSEL1 TO D-SRTSEL1
               MOVE SRTSEL2 TO D-SRTSEL2
               CALL "CBLRUN" USING DSORT-PAR PAR-SIZ
           END-IF
           IF  W-KBN = 13
               MOVE I-ID TO E-I-ID
               MOVE O-ID TO E-O-ID
               MOVE SRTKEY TO E-SRTKEY
               MOVE SRTOUT1 TO E-SRTOUT1
               MOVE SRTOUT2 TO E-SRTOUT2
               MOVE SRTSUM1 TO E-SRTSUM1
               MOVE SRTSUM2 TO E-SRTSUM2
               MOVE SRTSEL1 TO E-SRTSEL1
               MOVE SRTSEL2 TO E-SRTSEL2
               CALL "CBLRUN" USING ESORT-PAR PAR-SIZ
           END-IF
           CALL "DB_Close".
           STOP RUN.

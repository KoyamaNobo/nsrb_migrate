       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR600R.
       AUTHOR.                     OZAKI.
      *===============================================================*
      *    ï‚èïå≥í†ñ‚çáÇπ                                             *
      *                            --- 90/01/21 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
       77  WKZE                PIC  9(10) VALUE ZERO.
       77  WKSP                PIC  X(10) VALUE SPACE.
       77  WKNSP               PIC  N(20) VALUE SPACE.
       01  CRT-WK.
           02  W-01F.
             03  W-01FY        PIC  9(04).
             03  W-01FYL  REDEFINES W-01FY.
               04  W-01FY1     PIC  9(02).
               04  W-01FY2     PIC  9(02).
             03  W-01FM        PIC  9(02).
             03  W-01FD        PIC  9(02).
           02  W-01FR REDEFINES W-01F     PIC  9(08).
           02  W-01T.
             03  W-01TY        PIC  9(04).
             03  W-01TYL  REDEFINES W-01TY.
               04  W-01TY1     PIC  9(02).
               04  W-01TY2     PIC  9(02).
             03  W-01TM        PIC  9(02).
             03  W-01TD        PIC  9(02).
           02  W-01TR REDEFINES W-01T     PIC  9(08).
           02  W-01TL REDEFINES W-01T.
             03  F             PIC  9(02).
             03  W-01TS        PIC  9(06).
           02  W-02.
             03  W-021         PIC  9(04).
             03  W-022         PIC  9(04).
           02  W-03            PIC  9(02).
           02  W-OKC           PIC  X(01).
       01  CRT-ITEM.
           02  CRT-ITEM-TBL OCCURS 17.
             03  W-100.
               04  W-100M      PIC  9(02).
               04  W-100D      PIC  9(02).
             03  W-101         PIC  9(06).
             03  W-102         PIC  N(10).
             03  W-103         PIC S9(10).
             03  W-104         PIC S9(10).
             03  W-105         PIC S9(10).
             03  W-106         PIC  N(20).
             03  W-107         PIC  N(10).
             03  W-108         PIC  9(04).
             03  W-109         PIC  X(01).
       01  SW-AREA.
           02  W-SW            PIC  9(01) OCCURS 17.
       01  WORK-AREA.
           02  I               PIC  9(02).
           02  SOE             PIC  9(02).
           02  FI              PIC  9(02).
           02  TI              PIC  9(02).
           02  LIN             PIC  9(02).
           02  ERR-SW          PIC  9(01).
           02  INV-SW          PIC  9(01).
           02  W-ZAN           PIC S9(10).
           02  W-KASI          PIC S9(10).
           02  W-KARI          PIC S9(10).
           02  FROM-YMD.
             03  FROM-YM.
               04  FROM-Y      PIC  9(04).
               04  FROM-M      PIC  9(02).
             03  FROM-D        PIC  9(02).
           02  TO-YMD.
             03  TO-YM.
               04  TO-Y        PIC  9(04).
               04  TO-M        PIC  9(02).
             03  TO-D          PIC  9(02).
           02  TO-YMD1.
             03  TO-YM1.
               04  TO-Y1       PIC  9(04).
               04  TO-M1       PIC  9(02).
             03  TO-D1         PIC  9(02).
           02  W-YMD.
             03  W-Y           PIC  9(04).
             03  W-MD.
               04  W-M         PIC  9(02).
               04  W-D         PIC  9(02).
       COPY LWMSG.
       COPY LIBFDD.
       COPY KANGEL.
       COPY ACCUNT.
       COPY LHOZAN.
       COPY FCTL.
       01  SDH_PR600R.
           02  SDH_PNAME1      PIC  X(009)  VALUE "SIWAKE-H1".
           02  F               PIC  X(001).
           02  SDH_LNAME       PIC  X(003)  VALUE "SDH".
           02  F               PIC  X(001).
           02  SDH_KEY1        PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2        PIC  X(100)  VALUE SPACE.
           02  SDH_SORT        PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST       PIC  X(100)  VALUE SPACE.
           02  SDH_RES         USAGE  POINTER.
       COPY    SIWAKH.
       77      USER_ID         PIC  X(006) VALUE SPACE.
       77      COMPLETION_CODE PIC  X(003) VALUE ZERO.
       77      LINAGECOUNTER   PIC  9(003).
       77      ESTAT           PIC  X(002).
       77      RESU            PIC  9(001).
       77      RESP            PIC  9(001).
       77      RET             PIC  9(001) VALUE ZERO.
       01  DISP-C.
           02  DISP-CLE  PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  SP-ITEM.
           02  SP-IT  PIC  X(10)  VALUE  "CLEAR DATA".
       01  ACP-AREA.
           02  ACP-W01F.
               03  ACP-W01FY   PIC 9(02).
               03  ACP-W01FM   PIC 9(02).
               03  ACP-W01FD   PIC 9(02).
           02  ACP-W01T.
               03  ACP-W01TY   PIC 9(02).
               03  ACP-W01TM   PIC 9(02).
               03  ACP-W01TD   PIC 9(02).
           02  ACP-W021        PIC 9(04).
           02  ACP-W022        PIC 9(04).
           02  ACP-W03         PIC 9(02).
           02  ACP-WOKC        PIC X(01).
       01  DSP-AREA.
           02  DSP-W02N        PIC N(10).
       01  DSP-ITEM-AREA.
           02  DSP-I           PIC Z(02).
           02  DSP-W100.
               03  DSP-W100M   PIC Z9.
               03  DSP-W100MH  PIC X(01) VALUE "/".
               03  DSP-W100D   PIC Z9.
           02  DSP-W101        PIC 9(06).
           02  DSP-W102        PIC N(10).
           02  DSP-W103        PIC -----,---,---.
           02  DSP-W104        PIC -----,---,---.
           02  DSP-W105        PIC -----,---,--9.
       01  DSP-AREA1.
           02  DSP-W106        PIC N(20).
           02  DSP-W107        PIC N(10).
           02  DSP-W108        PIC 9(04).
           02  DSP-W109        PIC X(01).
       01  MG-AREA.
           02  MG-01           PIC N(07) VALUE "ì˙ïtä˙ä‘ÉGÉâÅ[".
       01  DSP-SP-AREA.
           02  SP-W03          PIC Z(02).
           02  SP-W106         PIC N(20).
           02  SP-W107         PIC N(10).
           02  SP-W108         PIC X(04).
           02  SP-W109         PIC X(01).
           02  SP-WOKC         PIC X(01).
       COPY LSMSG_PR.
       COPY LIBSCR.
       PROCEDURE                   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *    01  DISP-C
       CALL "SD_Init" USING
            "DISP-C" " " "1" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C"  RETURNING RESU.
      *    01  SP-ITEM
       CALL "SD_Init" USING 
            "SP-ITEM" " " "6" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-IT" "X" "6" "22" "10" " " "SP-ITEM"  RETURNING RESU.
      *    01  ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "23" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01F" " " "3" "0" "6" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01FY" "9" "3" "10" "2" " " "ACP-W01F"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01FY" BY REFERENCE W-01FY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01FM" "9" "3" "14" "2" "ACP-W01FY" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01FM" BY REFERENCE W-01FM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01FD" "9" "3" "18" "2" "ACP-W01FM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01FD" BY REFERENCE W-01FD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01T" " " "3" "0" "6" "ACP-W01F" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01TY" "9" "3" "24" "2" " " "ACP-W01T"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01TY" BY REFERENCE W-01TY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01TM" "9" "3" "28" "2" "ACP-W01TY" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01TM" BY REFERENCE W-01TM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W01TD" "9" "3" "32" "2" "ACP-W01TM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W01TD" BY REFERENCE W-01TD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W021" "9" "4" "11" "4" "ACP-W01T" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W021" BY REFERENCE W-021 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W022" "9" "4" "16" "4" "ACP-W021" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W022" BY REFERENCE W-022 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-W03" "9" "23" "2" "2" "ACP-W022" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-W03" BY REFERENCE W-03 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-WOKC" "X" "24" "77" "1" "ACP-W03" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-WOKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *    01  DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W02N" "N" "4" "21" "20" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W02N" BY REFERENCE KNGNMN "20" "0" RETURNING RESU.
      *    01  DSP-ITEM-AREA
       CALL "SD_Init" USING
            "DSP-ITEM-AREA" " " "0" "0" "72" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-I" "Z" "LIN" "2" "2" " " "DSP-ITEM-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-I" BY REFERENCE I "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W100" " " "LIN" "0" "5" "DSP-I" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W100M" "Z9" "LIN" "5" "2" " " "DSP-W100"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W100M" BY REFERENCE W-100M(1) "2" "1"
            BY REFERENCE I 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W100MH" "X" "LIN" "7" "1" "DSP-W100M" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W100D" "Z9" "LIN" "8" "2" "DSP-W100MH" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W100D" BY REFERENCE W-100D(1) "2" "1"
            BY REFERENCE I 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W101" "9" "LIN" "11" "6" "DSP-W100" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W101" BY REFERENCE W-101(1) "6" "1"
            BY REFERENCE I 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W102" "N" "LIN" "18" "20" "DSP-W101" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W102" BY REFERENCE W-107(1) "20" "1"
            BY REFERENCE I 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W103" "-----,---,---" "LIN" "39" "13" "DSP-W102" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W103" BY REFERENCE W-103(1) "10" "1"
            BY REFERENCE I 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W104" "-----,---,---" "LIN" "53" "13" "DSP-W103" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W104" BY REFERENCE W-104(1) "10" "1"
            BY REFERENCE I 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W105" "-----,---,--9" "LIN" "67" "13" "DSP-W104" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W105" BY REFERENCE W-105(1) "10" "1"
            BY REFERENCE I 115 RETURNING RESU.
      *    01  DSP-AREA1
       CALL "SD_Init" USING
            "DSP-AREA1" " " "0" "0" "65" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W106" "N" "23" "10" "40" " " "DSP-AREA1"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W106" BY REFERENCE W-106(1) "40" "1"
            BY REFERENCE W-03 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W107" "N" "23" "51" "20" "DSP-W106" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W107" BY REFERENCE W-102(1) "20" "1"
            BY REFERENCE W-03 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W108" "9" "23" "72" "4" "DSP-W107" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W108" BY REFERENCE W-108(1) "4" "1"
            BY REFERENCE W-03 115 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-W109" "X" "23" "77" "1" "DSP-W108" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-W109" BY REFERENCE W-109(1) "1" "1"
            BY REFERENCE W-03 115 RETURNING RESU.
      *    01  MG-AREA
       CALL "SD_Init" USING
            "MG-AREA" " " "0" "0" "14" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "MG-01" "N" "24" "2" "14" " " "MG-AREA"  RETURNING RESU.
      *    01  DSP-SP-AREA
       CALL "SD_Init" USING
            "DSP-SP-AREA" " " "0" "0" "68" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "SP-W03" "Z" "23" "2" "2" " " "DSP-SP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "SP-W03" BY REFERENCE WKZE "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "SP-W106" "N" "23" "10" "40" "SP-W03" " "  RETURNING RESU.
       CALL "SD_From" USING
            "SP-W106" BY REFERENCE WKNSP "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "SP-W107" "N" "23" "51" "20" "SP-W106" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "SP-W107" BY REFERENCE WKNSP "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "SP-W108" "X" "23" "72" "4" "SP-W107" " "  RETURNING RESU.
       CALL "SD_From" USING
            "SP-W108" BY REFERENCE WKSP "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "SP-W109" "X" "23" "77" "1" "SP-W108" " "  RETURNING RESU.
       CALL "SD_From" USING
            "SP-W109" BY REFERENCE WKSP "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "SP-WOKC" "X" "24" "77" "1" "SP-W109" " "  RETURNING RESU.
       CALL "SD_From" USING
            "SP-WOKC" BY REFERENCE WKSP "10" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MR000.
           PERFORM INI-RTN THRU INI-EX.
           IF  ERR-SW = 1
               GO TO MR999
           END-IF.
           PERFORM OPEN-RTN THRU OPEN-EX.
       MR010.
           PERFORM W01F-RTN THRU W01F-EX.
           IF  ESTAT = "P9"
               GO TO MR900
           END-IF.
       MR020.
           PERFORM W01T-RTN THRU W01T-EX.
           IF  ESTAT = "P9"
               GO TO MR900
           END-IF.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
       MR030.
           PERFORM W02-RTN THRU W02-EX.
           IF  ESTAT = "P9"
               GO TO MR900
           END-IF.
           IF  ESTAT = "09"
               GO TO MR020
           END-IF.
           MOVE W-02     TO KNG-KEY.
           PERFORM KNGG-RTN THRU KNGG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING
                    "INV-M01" INV-M01 "p" RETURNING RESU
               GO TO MR030
           END-IF.
           CALL "SD_Output" USING
            "DSP-W02N" DSP-W02N "p" RETURNING RESU.
      *
           MOVE W-021    TO AM-KEY.
           PERFORM AMG-RTN THRU AMG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO TO MR030
           END-IF.
           PERFORM HZMG-RTN THRU HZMG-EX.
      *
           MOVE ZERO     TO SW-AREA W-ZAN W-KARI W-KASI.
      *
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
      *
           MOVE ZERO     TO SH-KEY3.
           MOVE W-02     TO HKACD1.
           MOVE Z-TOUF(TI)     TO HTRDATE.
      *           START SDH KEY NOT LESS SH-KEY3 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY3" " NOT LESS " SH-KEY3 RETURNING RET.
           IF  RET = 1
               GO TO MR050
           END-IF.
       MR040.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR050
           END-IF.
           IF  W-02 NOT = HKACD1
               GO TO MR050
           END-IF.
           IF  W-01T < HTRDATE2
               GO TO MR050
           END-IF.
           IF  W-01F > HTRDATE2
               PERFORM SET-RTN THRU SET-EX
               GO TO MR040
           END-IF.
           MOVE 1     TO I.
           GO TO MR070.
       MR050.
           PERFORM ZAN-DSP-RTN THRU ZAN-DSP-EX.
           GO TO MR090.
       MR060.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR080
           END-IF.
           IF  W-02 NOT = HKACD1
               GO TO MR080
           END-IF.
           IF  W-01T < HTRDATE2
               GO TO MR080
           END-IF.
       MR070.
           PERFORM MOVE-RTN THRU MOVE-EX.
           IF  I NOT = 17
               ADD 1     TO I
               GO TO MR060
           END-IF.
       MR080.
           IF  W-SW(1) = 0
               GO TO MR110
           END-IF.
           PERFORM DISP-RTN THRU DISP-EX.
       MR090.
           PERFORM WOKC-RTN THRU WOKC-EX.
           IF  ESTAT = "09"
               GO TO MR100
           END-IF.
           CALL "SD_Output" USING
            "DSP-SP-AREA" DSP-SP-AREA "p" RETURNING RESU.
           IF W-OKC = "9"
              GO TO MR110
           END-IF.
           CALL "SD_Output" USING
            "SP-ITEM" SP-ITEM "p" RETURNING RESU.
           IF  W-SW(1) = 0
               GO TO MR110
           END-IF.
           IF  W-SW(17) = 0
               GO TO MR110
           END-IF.
      *
           MOVE ZERO     TO SW-AREA.
           MOVE 1        TO I.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR110
           END-IF.
           IF  W-02 NOT = HKACD1
               GO TO MR110
           END-IF.
           IF  W-01T < HTRDATE2
               GO TO MR110
           END-IF.
           GO TO MR070.
       MR100.
           PERFORM W03-RTN THRU W03-EX.
           IF  ESTAT = "09"
               CALL "SD_Output" USING
                "DSP-SP-AREA" DSP-SP-AREA "p" RETURNING RESU
               GO TO MR110
           END-IF.
           IF  W-03 = ZERO
               CALL "SD_Output" USING
                "DSP-SP-AREA" DSP-SP-AREA "p" RETURNING RESU
               GO TO MR090
           END-IF.
           PERFORM W106-RTN THRU W106-EX.
           PERFORM W107-RTN THRU W107-EX.
           PERFORM W108-RTN THRU W108-EX.
           PERFORM W109-RTN THRU W109-EX.
           GO TO MR100.
       MR110.
           CALL "SD_Output" USING
            "SP-ITEM" SP-ITEM "p" RETURNING RESU.
           GO TO MR030.
       MR900.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       MR999.
           CALL "DB_Close".
           STOP RUN.
       INI-RTN.
           CALL "SD_Screen_Output" USING "GR6000" RETURNING RESU.
           MOVE 0     TO ERR-SW.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  "     TO FCTL-KEY1 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "FCTL-F"     TO ERR-F
               MOVE "G"          TO ERR-M
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           MOVE FCTL-REC1     TO Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           MOVE Z-TOUT(Z-KSMM)     TO TO-YMD.
           MOVE Z-TOUT(15)         TO TO-YMD1.
           MOVE Z-UPDYM            TO FROM-YM.
           MOVE 01                 TO FROM-D.
           MOVE 1     TO I.
       INI-000.
           IF (Z-TOUF(I) NOT > FROM-YMD) AND
              (Z-TOUT(I) NOT < FROM-YMD)
               GO TO INI-100
           END-IF.
           IF  I NOT = 15
               ADD 1     TO I
               GO TO INI-000
           END-IF.
           IF  Z-KSMM = 12
               MOVE 1     TO I
           ELSE
               COMPUTE I = Z-KSMM + 1
           END-IF.
           MOVE Z-TOUF(I)              TO FROM-YMD.
           GO TO INI-EX.
       INI-100.
           IF  I = Z-KSMM
               MOVE 13     TO I
           ELSE
               IF  I = 12
                   MOVE 1     TO I
               ELSE
                   ADD 1     TO I
               END-IF
           END-IF.
           IF  I > 15
               CALL "SD_Output" USING
                "MG-01" MG-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           MOVE Z-TOUF(I)     TO FROM-YMD.
       INI-EX.
           EXIT.
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING 
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HZM-F_PNAME1 "SHARED" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY3" BY REFERENCE SH-KEY3.
           COPY LIBCPR.
       OPEN-EX.
           EXIT.
       W01F-RTN.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-W01F "ACP-W01F" " " "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO W01F-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W01F-RTN
           END-IF.
           MOVE ZERO TO W-01FY1.
           IF  W-01F = ZERO
               MOVE FROM-YMD     TO W-01F
               CALL "SD_Output" USING
                "ACP-W01F" ACP-W01F "p" RETURNING RESU
               GO TO W01F-050
           END-IF.
           IF  W-01FY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-01FY
           END-IF.
           IF  W-01FY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-01FY
           END-IF.
       W01F-050.
           IF  W-01F < FROM-YMD OR > TO-YMD1
               CALL "SD_Output" USING
                "MG-01" MG-01 "p" RETURNING RESU
               GO TO W01F-RTN
           END-IF.
           MOVE 1     TO I.
       W01F-100.
           IF (Z-TOUF(I) NOT > W-01F) AND
              (Z-TOUT(I) NOT < W-01F)
               CONTINUE
           ELSE
               IF  I NOT = 15
                   ADD 1     TO I
                   GO TO W01F-100
               ELSE
                   CALL "SD_Output" USING
                    "ERR-01" ERR-01 "p" RETURNING RESU
                   GO TO W01F-RTN
               END-IF
           END-IF.
           MOVE I     TO TI.
      *
           IF  Z-KSMM = 12
               MOVE 1     TO FI
           ELSE
               COMPUTE FI = Z-KSMM + 1
           END-IF.
           IF  TI > 12
               MOVE 13     TO FI
           END-IF.
       W01F-EX.
           EXIT.
       W01T-RTN.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-W01T "ACP-W01T" " " "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO W01T-EX
           END-IF.
           IF  ESTAT = "09"
               GO TO W01T-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W01T-RTN
           END-IF.
           MOVE ZERO TO W-01TY1.
           IF  W-01TS = 999999
               IF  TI < 13
                   MOVE TO-YMD     TO W-01T
               ELSE
                   MOVE TO-YMD1    TO W-01T
               END-IF
           END-IF.
           IF  W-01T = TO-YMD OR TO-YMD1
               CALL "SD_Output" USING
                "ACP-W01T" ACP-W01T "p" RETURNING RESU
               GO TO W01T-100
           END-IF.
           IF  W-01TY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-01TY
           END-IF.
           IF  W-01TY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-01TY
           END-IF.
       W01T-100.
           IF  TI < 13
               IF  W-01T > TO-YMD
                   CALL "SD_Output" USING
                    "MG-01" MG-01 "p" RETURNING RESU
                   GO TO W01T-RTN
               END-IF
           END-IF.
           IF  TI > 12
               IF  W-01T > TO-YMD1
                   CALL "SD_Output" USING
                    "MG-01" MG-01 "p" RETURNING RESU
                   GO TO W01T-RTN
               END-IF
           END-IF.
           IF  W-01T < W-01F
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               GO TO W01T-RTN
           END-IF.
       W01T-EX.
           EXIT.
       W02-RTN.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-W021 "ACP-W021" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO W02-EX
           END-IF.
           IF  ESTAT = "09"
               GO TO W02-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W02-RTN
           END-IF.
       W02-000.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-W022 "ACP-W022" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W02-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W02-000
           END-IF.
           IF  W-022 = ZERO
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               GO TO W02-000
           END-IF.
       W02-EX.
           EXIT.
       KNGG-RTN.
           MOVE 0     TO INV-SW.
      *           READ KNG UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE     TO KNGNMN
               MOVE 1         TO INV-SW
           END-IF.
       KNGG-EX.
           EXIT.
       AMG-RTN.
           MOVE 0        TO INV-SW.
           MOVE W-021    TO AM-KEY.
      *           READ AM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
           END-IF.
       AMG-EX.
           EXIT.
       HZMG-RTN.
           MOVE 0        TO INV-SW.
           MOVE W-02     TO HZM-KEY.
      *           READ HZM-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HZM-F_PNAME1 BY REFERENCE HZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE HZM-R
               MOVE 1     TO INV-SW
           END-IF.
       HZMG-EX.
           EXIT.
       ZAN-SET-RTN.
           IF  TI > 12
               GO TO ZAN-SET-500
           END-IF.
           MOVE FI     TO I.
       ZAN-SET-000.
           ADD HZM-TJKR(I)     TO W-KARI.
           ADD HZM-TJKS(I)     TO W-KASI.
           IF  I = TI
               GO TO ZAN-SET-900
           END-IF.
           IF  I = 12
               MOVE 1     TO I
               GO TO ZAN-SET-000
           END-IF.
           ADD 1     TO I.
           GO TO ZAN-SET-000.
       ZAN-SET-500.
           IF  BS-PL = 0
               MOVE 1      TO I
           ELSE
               MOVE 13     TO I
           END-IF.
       ZAN-SET-600.
           ADD HZM-TJKR(I)     TO W-KARI.
           ADD HZM-TJKS(I)     TO W-KASI.
           IF  I = TI
               GO TO ZAN-SET-900
           END-IF
           IF  I = 15
               GO TO ZAN-SET-900
           END-IF.
           ADD 1     TO I.
           GO TO ZAN-SET-600.
       ZAN-SET-900.
           IF  DR-CR = 1
               COMPUTE W-ZAN = HZM-ZAN + (W-KARI - HZM-TJKR(TI)) -
                               ( W-KASI - HZM-TJKS(TI))
           ELSE
               COMPUTE W-ZAN = HZM-ZAN + (W-KASI - HZM-TJKS(TI)) -
                               ( W-KARI - HZM-TJKR(TI))
           END-IF.
       ZAN-SET-EX.
           EXIT.
       SET-RTN.
           IF  DR-CR = HDR-CR
               COMPUTE W-ZAN = W-ZAN + HAMOUNT
           ELSE
               COMPUTE W-ZAN = W-ZAN - HAMOUNT
           END-IF.
       SET-EX.
           EXIT.
       ZAN-DSP-RTN.
           MOVE 1     TO I.
           MOVE W-ZAN     TO W-105(I).
           MOVE 6     TO LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-W105" DSP-W105 "p" RETURNING RESU.
       ZAN-DSP-EX.
           EXIT.
       MOVE-RTN.
           MOVE HTRDATE     TO W-YMD.
           MOVE W-MD        TO W-100(I).
           MOVE HJUNLNO     TO W-101(I).
           IF  HOPPCD = ZERO
               MOVE "èîå˚"     TO W-102(I)
           ELSE
               MOVE HOPPCD     TO K-ACCD
               MOVE ZERO       TO K-HOCD
               PERFORM KNGG-RTN THRU KNGG-EX
               MOVE KNGNMN     TO W-102(I)
           END-IF.
           MOVE 0           TO W-103(I) W-104(I).
           IF  HDR-CR  = 1
               MOVE HAMOUNT     TO W-103(I)
           ELSE
               MOVE HAMOUNT     TO W-104(I)
           END-IF.
           PERFORM SET-RTN THRU SET-EX.
           MOVE W-ZAN     TO W-105(I).
           MOVE HTEKIYO     TO W-106(I).
           MOVE HNAMEN      TO W-107(I).
           MOVE HSECTCD     TO W-108(I).
           MOVE " "         TO W-109(I).
           IF  HTAXKB     =  "1"  OR  "5"
               MOVE "*"         TO W-109(I)
           END-IF.
           IF  HTAXKB     =  "3"  OR  "7"
               MOVE "#"         TO W-109(I)
           END-IF.
           IF  HTAXKB     =  " "
               IF  HETAX      =  "1"  OR  "5"
                   MOVE "*"         TO W-109(I)
               END-IF
           END-IF.
           IF  HTAXKB     =  " "
               IF  HETAX      =  "3"  OR  "7"
                   MOVE "#"         TO W-109(I)
               END-IF
           END-IF.
           MOVE 1     TO W-SW(I).
       MOVE-EX.
           EXIT.
       DISP-RTN.
           MOVE 1     TO I.
       DISP-000.
           IF  W-SW(I) = 0
               GO TO DISP-EX
           END-IF.
           COMPUTE LIN = I + 5.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-ITEM-AREA" DSP-ITEM-AREA "p" RETURNING RESU.
           IF  I NOT = 17
               ADD 1     TO I
               GO TO DISP-000
           END-IF.
       DISP-EX.
           EXIT.
      *********
       WOKC-RTN.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-WOKC "ACP-WOKC" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = "09"
               GO TO WOKC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO WOKC-RTN
           END-IF.
           IF  W-OKC NOT = "1" AND "9"
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               GO TO WOKC-RTN
           END-IF.
       WOKC-EX.
           EXIT.
       W03-RTN.
           IF  W-SW(1) = 0
               GO TO W03-EX
           END-IF.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-W03 "ACP-W03" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W03-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W03-RTN
           END-IF.
           IF  W-03 = ZERO
               CALL "SD_Output" USING
                "DSP-SP-AREA" DSP-SP-AREA "p" RETURNING RESU
               GO TO W03-EX
           END-IF.
           IF  W-03 > 17
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               GO TO W03-RTN
           END-IF.
           IF  W-SW(W-03) = 0
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               GO TO W03-RTN
           END-IF.
           CALL "SD_Output" USING
            "SP-W106" SP-W106 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "SP-W107" SP-W107 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "SP-W108" SP-W108 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "SP-W109" SP-W109 "p" RETURNING RESU.
       W03-EX.
           EXIT.
       W106-RTN.
           CALL "SD_Output" USING
            "DSP-W106" DSP-W106 "p" RETURNING RESU.
       W106-EX.
           EXIT.
       W107-RTN.
           CALL "SD_Output" USING
            "DSP-W107" DSP-W107 "p" RETURNING RESU.
       W107-EX.
           EXIT.
       W108-RTN.
           CALL "SD_Output" USING
            "DSP-W108" DSP-W108 "p" RETURNING RESU.
       W108-EX.
           EXIT.
       W109-RTN.
           CALL "SD_Output" USING
            "DSP-W109" DSP-W109 "p" RETURNING RESU.
       W109-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HZM-F_IDLST HZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "SD_Output" USING
            "DISP-C" DISP-C "p" RETURNING RESU.
       CLSE-EXT.
           EXIT.
       COPY LPMSG_PR.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT450.
      *********************************************************
      *    PROGRAM         :  分類別販売実績・在庫問合せ　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT45                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  W-SEN              PIC  9(001).
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-MDD.
           02  W-MD    OCCURS 57.
             03  WM-TM        PIC  X(030).
             03  WM-MD    REDEFINES WM-TM.
               04  WM-M0      PIC  N(003).
               04  F          PIC  X(001).
               04  WM-M1      PIC  N(003).
               04  F          PIC  X(001).
               04  WM-M2      PIC  N(008).
             03  WM-SS        PIC S9(006).
             03  WM-UKN       PIC S9(009).
             03  WM-UG        PIC S9(009).
             03  WM-UR        PIC S9(009).
             03  WM-YKS       PIC S9(006).
             03  WM-YKK       PIC S9(009).
       01  W-DATA.
           02  W-SCD.
             03  W-CC         PIC  9(002).
             03  W-NC         PIC  9(002).
             03  W-SC         PIC  9(002).
           02  W-D.
             03  W-UR         PIC S9(009).
             03  W-SS         PIC S9(007).
             03  W-UKN        PIC S9(010).
             03  W-UG         PIC S9(010).
             03  W-YKS        PIC S9(007).
             03  W-YKK        PIC S9(010).
           02  WT-D.
             03  WT-SS        PIC S9(007).
             03  WT-UKN       PIC S9(010).
             03  WT-UG        PIC S9(010).
             03  WT-YKS       PIC S9(007).
             03  WT-YKK       PIC S9(010).
           02  WS-D.
             03  WS-SS        PIC S9(007).
             03  WS-UKN       PIC S9(010).
             03  WS-UG        PIC S9(010).
             03  WS-YKS       PIC S9(007).
             03  WS-YKK       PIC S9(010).
           02  WA-D.
             03  WA-SS        PIC S9(007).
             03  WA-UKN       PIC S9(010).
             03  WA-UG        PIC S9(010).
             03  WA-YKS       PIC S9(007).
             03  WA-YKK       PIC S9(010).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-BRN3         PIC  N(003).
           02  W-BMN          PIC  N(003).
           02  W-L            PIC  9(002).
           02  W-DC           PIC  9(001).
           02  W-C            PIC  9(002).
           02  W-CD           PIC  9(002).
           02  W-NGP.
             03  F            PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
      *FD  HC-F
       01  HC-F_HMT450.
           02  HC-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HC-F_LNAME     PIC  X(011) VALUE "HC-F_HMT450".
           02  F              PIC  X(001).
           02  HC-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HC-F_SORT      PIC  X(100) VALUE SPACE.
           02  HC-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HC-F_RES       USAGE  POINTER.
       01  HC-R.
           02  HC-BC.
             03  HC-BC1       PIC  9(002).
             03  HC-BC2.
               04  HC-BC21    PIC  9(001).
               04  HC-BC22    PIC  9(001).
             03  HC-BC3       PIC  9(002).
             03  HC-BMC       PIC  9(002).
             03  HC-BMNO      PIC  9(001).
           02  HC-ZKS         PIC S9(007).
           02  HC-ZKK         PIC S9(010).
           02  HC-NS          PIC S9(007).
           02  HC-SKN         PIC S9(010).
           02  HC-SS          PIC S9(007).
           02  HC-UKN         PIC S9(010).
           02  HC-YKS         PIC S9(007).
           02  HC-YKK         PIC S9(010).
           02  HC-UG          PIC S9(010).
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
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　履物分類別　販売・在庫　問合せ　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(028) VALUE
                "販売 = 0  ,  在庫 = 1  ...  ".
       01  C-ACP.
           02  A-SEN    PIC  X(001).
           02  A-DMM    PIC  X(001).
       01  C-DSP.
           02  D-HHD.
             03  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　履物分類別　販売実績表　　＊＊＊".
             03  FILLER  PIC  X(047) VALUE
                  "販売足数     売上金額     販売原価     販売利益".
           02  D-ZHD.
             03  FILLER  PIC  N(021) VALUE
                  "　＊＊＊　　履物分類別　在庫表　　＊＊＊　".
             03  FILLER  PIC  X(047) VALUE
                  "在庫足数     在庫金額                          ".
           02  D-GP.
             03  01D-GP  PIC Z9 .
             03  02D-GP  PIC Z9 .
           02  D-HMD.
             03  01D-HMD PIC  X(030).
             03  02D-HMD PIC ----,--9 .
             03  03D-HMD PIC ----,---,--9 .
             03  04D-HMD PIC ----,---,--9 .
             03  05D-HMD PIC ----,---,--9 .
           02  D-ZMD.
             03  01D-ZMD PIC  X(030).
             03  02D-ZMD PIC ----,--9 .
             03  03D-ZMD PIC ----,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  ﾌﾞﾝﾙｲ ｶﾞ 57ｦ ｺｴﾀ  ***".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "378" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "12" "21" "28" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "X" "12" "48" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "X" "23" "79" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "306" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-HHD" " " "0" "0" "89" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-HHD" "N" "1" "20" "42" " " "D-HHD" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-HHD" "X" "3" "33" "47" "01D-HHD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-ZHD" " " "0" "0" "89" "D-HHD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-ZHD" "N" "1" "20" "42" " " "D-ZHD" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-ZHD" "X" "3" "33" "47" "01D-ZHD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-GP" " " "2" "0" "4" "D-ZHD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-GP" "Z9" "2" "68" "2" " " "D-GP" RETURNING RESU.
       CALL "SD_From" USING
           "01D-GP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-GP" "Z9" "2" "72" "2" "01D-GP" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-GP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-HMD" " " "W-L" "0" "74" "D-GP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-HMD" "X" "W-L" "2" "30" " " "D-HMD" RETURNING RESU.
       CALL "SD_From" USING
           "01D-HMD" BY REFERENCE WM-TM(1) "30" "1" BY REFERENCE W-C 78
            RETURNING RESU.
       CALL "SD_Init" USING
           "02D-HMD" "----,--9" "W-L" "33" "8" "01D-HMD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-HMD" BY REFERENCE WM-SS(1) "6" "1" BY REFERENCE W-C 78
            RETURNING RESU.
       CALL "SD_Init" USING
           "03D-HMD" "----,---,--9" "W-L" "42" "12" "02D-HMD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-HMD" BY REFERENCE WM-UKN(1) "9" "1" BY REFERENCE W-C 78
            RETURNING RESU.
       CALL "SD_Init" USING
           "04D-HMD" "----,---,--9" "W-L" "55" "12" "03D-HMD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-HMD" BY REFERENCE WM-UG(1) "9" "1" BY REFERENCE W-C 78
            RETURNING RESU.
       CALL "SD_Init" USING
           "05D-HMD" "----,---,--9" "W-L" "68" "12" "04D-HMD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "05D-HMD" BY REFERENCE WM-UR(1) "9" "1" BY REFERENCE W-C 78
            RETURNING RESU.
       CALL "SD_Init" USING
           "D-ZMD" " " "W-L" "0" "50" "D-HMD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-ZMD" "X" "W-L" "2" "30" " " "D-ZMD" RETURNING RESU.
       CALL "SD_From" USING
           "01D-ZMD" BY REFERENCE WM-TM(1) "30" "1" BY REFERENCE W-C 78
            RETURNING RESU.
       CALL "SD_Init" USING
           "02D-ZMD" "----,--9" "W-L" "33" "8" "01D-ZMD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-ZMD" BY REFERENCE WM-YKS(1) "6" "1" BY REFERENCE W-C 78
            RETURNING RESU.
       CALL "SD_Init" USING
           "03D-ZMD" "----,---,--9" "W-L" "42" "12" "02D-ZMD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-ZMD" BY REFERENCE WM-YKK(1) "9" "1" BY REFERENCE W-C 78
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "53" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HC-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HC-F_PNAME1 " " BY REFERENCE HC-F_IDLST "0".
       M-10.
      *           READ HC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HC-F_PNAME1 BY REFERENCE HC-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HC-F_IDLST HC-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = HC-ZKS AND HC-ZKK AND HC-NS AND HC-SKN AND
                     HC-SS AND HC-UKN AND HC-YKS AND HC-YKK AND HC-UG
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
      *
           PERFORM S-40 THRU S-50.
           MOVE D-HSD TO W-NGP.
           MOVE ZERO TO WA-D W-C.
       M-15.
           MOVE HC-BC3 TO W-CC.
           MOVE ZERO TO WS-D CHK.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-CC TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN3
           END-IF
           MOVE HKB-BRN3 TO W-BRN3.
       M-20.
           MOVE HC-BMC TO W-NC.
           MOVE ZERO TO WT-D CHK2 CNT.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-NC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO W-BMN.
       M-25.
           ADD 1 TO W-C.
           IF  W-C > 57
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-BRN3 TO WM-M0(W-C)
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-BMN TO WM-M1(W-C)
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE HC-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-BRN1
           END-IF
           MOVE HKB-BRN1 TO WM-M2(W-C).
      *
           MOVE ZERO TO W-D.
           MOVE HC-SS TO W-SS.
           MOVE HC-UKN TO W-UKN.
           MOVE HC-UG TO W-UG.
           MOVE HC-YKS TO W-YKS.
           MOVE HC-YKK TO W-YKK.
      *
           PERFORM S-05 THRU S-10.
      *
           ADD HC-SS TO WT-SS.
           ADD HC-UKN TO WT-UKN.
           ADD HC-UG TO WT-UG.
           ADD HC-YKS TO WT-YKS.
           ADD HC-YKK TO WT-YKK.
           IF  CNT < 2
               ADD 1 TO CNT
           END-IF.
       M-30.
      *           READ HC-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HC-F_PNAME1 BY REFERENCE HC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  ZERO = HC-ZKS AND HC-ZKK AND HC-NS AND HC-SKN AND
                     HC-SS AND HC-UKN AND HC-YKS AND HC-YKK AND HC-UG
               GO TO M-30
           END-IF
           IF  W-CC NOT = HC-BC3
               GO TO M-35
           END-IF
           IF  W-NC = HC-BMC
               GO TO M-25
           END-IF
           PERFORM S-15 THRU S-25.
           IF  W-END = 1
               GO TO M-95
           END-IF
           GO TO M-20.
       M-35.
           PERFORM S-15 THRU S-25.
           IF  W-END = 1
               GO TO M-95
           END-IF
           PERFORM S-30 THRU S-35.
           IF  W-END = 1
               GO TO M-95
           END-IF
           GO TO M-15.
       M-40.
           PERFORM S-15 THRU S-25.
           IF  W-END = 1
               GO TO M-95
           END-IF
           PERFORM S-30 THRU S-35.
           IF  W-END = 1
               GO TO M-95
           END-IF
      *
           ADD 1 TO W-C.
           IF  W-C > 57
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE "    【　合　計　】            " TO WM-TM(W-C).
           MOVE ZERO TO W-D.
           MOVE WA-SS TO W-SS.
           MOVE WA-UKN TO W-UKN.
           MOVE WA-UG TO W-UG.
           MOVE WA-YKS TO W-YKS.
           MOVE WA-YKK TO W-YKK.
           PERFORM S-05 THRU S-10.
           MOVE 0 TO W-DC.
       M-45.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           IF  W-SEN > 1
               GO TO M-50
           END-IF
      *
           IF  W-DC = 0
               MOVE 1 TO W-DC
               MOVE W-C TO W-CD
           END-IF.
       M-55.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT45" RETURNING RESU.
           IF  W-SEN = 0
               CALL "SD_Output" USING "D-HHD" D-HHD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-ZHD" D-ZHD "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
           MOVE ZERO TO W-C.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-60.
           ADD 1 TO W-C W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-CD < W-C
               GO TO M-70
           END-IF
           IF  W-C = 20 OR 39
               GO TO M-70
           END-IF.
       M-65.
           IF  W-SEN = 0
               CALL "SD_Output" USING "D-HMD" D-HMD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-ZMD" D-ZMD "p" RETURNING RESU
           END-IF
           GO TO M-60.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-45
           END-IF
           IF  ESTAT = PF7
               IF ((W-C <= 20) AND (W-CD > 19)) OR
                  ((W-C <= 39) AND (W-CD > 38))
                   GO TO M-75
               END-IF
           END-IF
           GO TO M-80.
       M-75.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT45" RETURNING RESU.
           IF  W-SEN = 0
               CALL "SD_Output" USING "D-HHD" D-HHD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-ZHD" D-ZHD "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO M-65.
       M-80.
           IF  ESTAT NOT = PF8
               GO TO M-70
           END-IF
           IF  W-C < 40
               IF  W-C > 20
                   GO TO M-55
               ELSE
                   GO TO M-70
               END-IF
           END-IF
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT45" RETURNING RESU.
           IF  W-SEN = 0
               CALL "SD_Output" USING "D-HHD" D-HHD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-ZHD" D-ZHD "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
           MOVE 20 TO W-C.
           GO TO M-65.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HC-F_IDLST HC-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE W-SS TO WM-SS(W-C).
           MOVE W-UKN TO WM-UKN(W-C).
           MOVE W-UG TO WM-UG(W-C).
           COMPUTE W-UR = W-UKN - W-UG.
           MOVE W-UR TO WM-UR(W-C).
           MOVE W-YKS TO WM-YKS(W-C).
           MOVE W-YKK TO WM-YKK(W-C).
       S-10.
           EXIT.
       S-15.
           IF  CNT NOT = 2
               GO TO S-20
           END-IF
           ADD 1 TO W-C.
           IF  W-C > 57
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 1 TO W-END
               GO TO S-25
           END-IF
           MOVE "　　　　　　　　（　計　）    " TO WM-TM(W-C).
           MOVE ZERO TO W-D.
           MOVE WT-SS TO W-SS.
           MOVE WT-UKN TO W-UKN.
           MOVE WT-UG TO W-UG.
           MOVE WT-YKS TO W-YKS.
           MOVE WT-YKK TO W-YKK.
      *
           PERFORM S-05 THRU S-10.
       S-20.
           ADD WT-SS TO WS-SS.
           ADD WT-UKN TO WS-UKN.
           ADD WT-UG TO WS-UG.
           ADD WT-YKS TO WS-YKS.
           ADD WT-YKK TO WS-YKK.
       S-25.
           EXIT.
       S-30.
           ADD 1 TO W-C.
           IF  W-C > 57
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 1 TO W-END
               GO TO S-35
           END-IF
           MOVE "　　　　［　小　計　］        " TO WM-TM(W-C).
           MOVE ZERO TO W-D.
           MOVE WS-SS TO W-SS.
           MOVE WS-UKN TO W-UKN.
           MOVE WS-UG TO W-UG.
           MOVE WS-YKS TO W-YKS.
           MOVE WS-YKK TO W-YKK.
           PERFORM S-05 THRU S-10.
      *
           ADD WS-SS TO WA-SS.
           ADD WS-UKN TO WA-UKN.
           ADD WS-UG TO WA-UG.
           ADD WS-YKS TO WA-YKS.
           ADD WS-YKK TO WA-YKK.
       S-35.
           EXIT.
       S-40.
           MOVE ZERO TO W-C.
       S-45.
           ADD 1 TO W-C.
           IF  W-C NOT = 58
               INITIALIZE W-MD(W-C)
               GO TO S-45
           END-IF.
       S-50.
           EXIT.

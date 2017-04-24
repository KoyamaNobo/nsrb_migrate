       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMH050.
      *********************************************************
      *    PROGRAM         :  履物在庫廃棄明細ワーク　作成  　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
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
           02  W-HCD          PIC  9(006).
           02  W-SU           PIC S9(006).
           02  CNT            PIC  9(002).
           02  W-DC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHIM.
           COPY LIHHTF.
      *FD  HAIKI
       01  HAIKI_HMH050.
           02  HAIKI_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HAIKI_LNAME    PIC  X(012) VALUE "HAIKI_HMH050".
           02  F              PIC  X(001).
           02  HAIKI_KEY1     PIC  X(100) VALUE SPACE.
           02  HAIKI_SORT     PIC  X(100) VALUE SPACE.
           02  HAIKI_IDLST    PIC  X(100) VALUE SPACE.
           02  HAIKI_RES      USAGE  POINTER.
       01  HAI-R.
           02  HAI-HCD        PIC  9(006).
           02  HAI-SIZ        PIC  9(001).
           02  HAI-ASUD.
             03  HAI-SUD   OCCURS  10.
               04  HAI-SU     PIC S9(005).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
      *FD  TSWF
       01  TSWF_HMH050.
           02  TSWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSWF_LNAME     PIC  X(011) VALUE "TSWF_HMH050".
           02  F              PIC  X(001).
           02  TSWF_KEY1      PIC  X(100) VALUE SPACE.
           02  TSWF_SORT      PIC  X(100) VALUE SPACE.
           02  TSWF_IDLST     PIC  X(100) VALUE SPACE.
           02  TSWF_RES       USAGE  POINTER.
       01  TSW-R.
           02  TSW-HCD        PIC  9(006).
           02  TSW-KSU        PIC S9(006).
           02  TSW-HSU        PIC S9(006).
           02  TSW-FT         PIC  9(005).
           02  TSW-BC1        PIC  9(002).
           02  TSW-BC2        PIC  9(002).
           02  TSW-BC3        PIC  9(002).
           02  TSW-BMC        PIC  9(002).
           02  TSW-BMNO       PIC  9(001).
           02  F              PIC  X(096).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物在庫廃棄ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME2   PIC  X(019) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-KEY   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "53" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "16" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "19" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "40" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO HAIKI_PNAME1.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO TSWF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" TSWF_PNAME1 " " BY REFERENCE TSWF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
      *
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE 0 TO W-DC.
       M-10.
           MOVE ZERO TO W-SU.
           MOVE HHT-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               GO TO M-90
           END-IF.
       M-15.
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT < 11
               ADD HHT-ZSU(CNT) TO W-SU
               GO TO M-20
           END-IF
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  HHT-HCD = W-HCD
               GO TO M-15
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-10.
       M-25.
           PERFORM S-05 THRU S-10.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HAIKI_PNAME1 " " BY REFERENCE HAIKI_IDLST "0".
       M-30.
      *           READ HAIKI AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HAIKI_PNAME1 BY REFERENCE HAI-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE HAIKI_IDLST HAIKI_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE 1 TO W-DC.
       M-35.
           MOVE ZERO TO W-SU.
           MOVE HAI-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HAIKI_IDLST HAIKI_PNAME1
               GO TO M-90
           END-IF.
       M-40.
           MOVE ZERO TO CNT.
       M-45.
           ADD 1 TO CNT.
           IF  CNT < 11
               ADD HAI-SU(CNT) TO W-SU
               GO TO M-45
           END-IF
      *           READ HAIKI AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HAIKI_PNAME1 BY REFERENCE HAI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  HAI-HCD = W-HCD
               GO TO M-40
           END-IF
           PERFORM S-05 THRU S-10.
           GO TO M-35.
       M-50.
           PERFORM S-05 THRU S-10.
           CALL "DB_F_Close" USING
            BY REFERENCE HAIKI_IDLST HAIKI_PNAME1.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSWF_IDLST TSWF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-SU = ZERO
               GO TO S-10
           END-IF
           MOVE ZERO TO TSW-R.
           MOVE W-HCD TO TSW-HCD.
           IF  W-DC = 0
               MOVE W-SU TO TSW-KSU
           ELSE
               MOVE W-SU TO TSW-HSU
           END-IF
           MOVE HI-FT TO TSW-FT.
           MOVE HI-BC1 TO TSW-BC1.
           MOVE HI-BC2 TO TSW-BC2.
           MOVE HI-BC3 TO TSW-BC3.
           MOVE HI-BMC TO TSW-BMC.
           MOVE HI-BMNO TO TSW-BMNO.
      *           WRITE TSW-R.
      *//////////////
           CALL "DB_Insert" USING
            TSWF_PNAME1 TSWF_LNAME TSW-R RETURNING RET.
       S-10.
           EXIT.

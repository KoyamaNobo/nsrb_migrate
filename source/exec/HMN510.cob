       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN510.
      *********************************************************
      *    PROGRAM         :  履物棚卸集計ワーク　作成　　　　*
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
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DC           PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  CNT            PIC  9(002).
           02  W-ZS           PIC S9(006).
           02  W-NS           PIC S9(006).
           02  W-US           PIC S9(006).
           02  W-AS           PIC S9(006).
           02  W-ASS          PIC S9(006).
           02  W-TZS          PIC S9(006).
           02  W-TSU          PIC S9(006).
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHHTF.
      *FD  TSWF
       01  TSWF_HMN510.
           02  TSWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSWF_LNAME     PIC  X(011) VALUE "TSWF_HMN510".
           02  F              PIC  X(001).
           02  TSWF_KEY1      PIC  X(100) VALUE SPACE.
           02  TSWF_SORT      PIC  X(100) VALUE SPACE.
           02  TSWF_IDLST     PIC  X(100) VALUE SPACE.
           02  TSWF_RES       USAGE  POINTER.
       01  TSW-R.
           02  TSW-HCD        PIC  9(006).
           02  TSW-TZS        PIC S9(006).
           02  TSW-TSU        PIC S9(006).
           02  TSW-FT         PIC  9(005).
           02  TSW-BC1        PIC  9(002).
           02  TSW-BC2        PIC  9(002).
           02  TSW-BC3        PIC  9(002).
           02  TSW-BMC        PIC  9(002).
           02  TSW-BMNO       PIC  9(001).
           02  F              PIC  X(032).
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
                "＊＊＊　　履物棚卸集計ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "子ｺｰﾄﾞ=0 , 親ｺｰﾄﾞ=1 ... ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK-1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME2   PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  9(006).
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
           "C-MID" " " "0" "0" "354" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "20" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "22" "21" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "15" "43" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "22" "38" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
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
           "E-ME1" "X" "24" "15" "16" "E-ME98" " " RETURNING RESU.
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
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN > 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
           COPY LIBCPR.
           IF  D-NHG = 5 OR 8 OR 11
               MOVE 0 TO W-DC
           ELSE
               MOVE 1 TO W-DC
           END-IF
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
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TSWF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" TSWF_PNAME1 " " BY REFERENCE TSWF_IDLST "0".
       M-20.
           IF  W-SEN = 0
               MOVE HHT-HCD TO W-HCD
           ELSE
               MOVE HHT-MHCD TO W-HCD
           END-IF
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
               GO TO M-90
           END-IF
           MOVE ZERO TO W-ASS.
       M-25.
           COMPUTE W-TSU = HHT-TSU(01) + HHT-TSU(02) + HHT-TSU(03)
                         + HHT-TSU(04) + HHT-TSU(05) + HHT-TSU(06)
                         + HHT-TSU(07) + HHT-TSU(08) + HHT-TSU(09)
                         + HHT-TSU(10).
           IF  W-DC = 0
               COMPUTE W-TZS = HHT-TZS(01) + HHT-TZS(02) + HHT-TZS(03)
                             + HHT-TZS(04) + HHT-TZS(05) + HHT-TZS(06)
                             + HHT-TZS(07) + HHT-TZS(08) + HHT-TZS(09)
                             + HHT-TZS(10)
               GO TO M-30
           END-IF
           IF  HHT-SIZ = 4
               COMPUTE W-ZS = HHT-ZSU(01) + HHT-ZSU(02) + HHT-ZSU(03)
                            + HHT-ZSU(04) + HHT-ZSU(05) + HHT-ZSU(06)
                            + HHT-ZSU(07) + HHT-ZSU(08) + HHT-ZSU(09)
               COMPUTE W-NS = HHT-NSU(01) + HHT-NSU(02) + HHT-NSU(03)
                            + HHT-NSU(04) + HHT-NSU(05) + HHT-NSU(06)
                            + HHT-NSU(07) + HHT-NSU(08) + HHT-NSU(09)
               COMPUTE W-US = HHT-USU(01) + HHT-USU(02) + HHT-USU(03)
                            + HHT-USU(04) + HHT-USU(05) + HHT-USU(06)
                            + HHT-USU(07) + HHT-USU(08) + HHT-USU(09)
               COMPUTE W-AS = HHT-ASS(01) + HHT-ASS(02) + HHT-ASS(03)
                            + HHT-ASS(04) + HHT-ASS(05) + HHT-ASS(06)
                            + HHT-ASS(07) + HHT-ASS(08) + HHT-ASS(09)
               ADD W-AS TO W-ASS
               COMPUTE W-TZS = W-ZS + W-NS - W-US - W-AS
               COMPUTE W-TZS = W-TZS + HHT-ZSU(10) + W-ASS - HHT-USU(10)
           ELSE
               COMPUTE W-ZS = HHT-ZSU(01) + HHT-ZSU(02) + HHT-ZSU(03)
                            + HHT-ZSU(04) + HHT-ZSU(05) + HHT-ZSU(06)
                            + HHT-ZSU(07) + HHT-ZSU(08) + HHT-ZSU(09)
                            + HHT-ZSU(10)
               COMPUTE W-NS = HHT-NSU(01) + HHT-NSU(02) + HHT-NSU(03)
                            + HHT-NSU(04) + HHT-NSU(05) + HHT-NSU(06)
                            + HHT-NSU(07) + HHT-NSU(08) + HHT-NSU(09)
                            + HHT-NSU(10)
               COMPUTE W-US = HHT-USU(01) + HHT-USU(02) + HHT-USU(03)
                            + HHT-USU(04) + HHT-USU(05) + HHT-USU(06)
                            + HHT-USU(07) + HHT-USU(08) + HHT-USU(09)
                            + HHT-USU(10)
               COMPUTE W-AS = HHT-ASS(01) + HHT-ASS(02) + HHT-ASS(03)
                            + HHT-ASS(04) + HHT-ASS(05) + HHT-ASS(06)
                            + HHT-ASS(07) + HHT-ASS(08) + HHT-ASS(09)
                            + HHT-ASS(10)
               ADD W-AS TO W-ASS
               COMPUTE W-TZS = W-ZS + W-NS - W-US - W-AS
           END-IF.
       M-30.
           IF  ZERO = W-TZS AND W-TSU
               GO TO M-35
           END-IF
           MOVE ZERO TO TSW-R.
           MOVE W-HCD TO TSW-HCD.
           MOVE W-TZS TO TSW-TZS.
           MOVE W-TSU TO TSW-TSU.
           MOVE HI-FT TO TSW-FT.
           MOVE HI-BC1 TO TSW-BC1.
           MOVE HI-BC2 TO TSW-BC2.
           MOVE HI-BC3 TO TSW-BC3.
           MOVE HI-BMC TO TSW-BMC.
           MOVE HI-BMNO TO TSW-BMNO.
      *           WRITE TSW-R.
      *///////////////
           CALL "DB_Insert" USING
            TSWF_PNAME1 TSWF_LNAME TSW-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-35.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-SEN = 0
               IF  HHT-HCD = W-HCD
                   GO TO M-25
               END-IF
           END-IF
           IF  W-SEN = 1
               IF  HHT-MHCD = W-HCD
                   GO TO M-25
               END-IF
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSWF_IDLST TSWF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

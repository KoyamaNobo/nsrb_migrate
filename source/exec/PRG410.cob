       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PRG410.
      ********************************************
      *****    月別消費税内訳ワーク　作成    *****
      ********************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       INPUT-OUTPUT      SECTION.
       DATA  DIVISION.
       WORKING-STORAGE       SECTION.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-SED.
             03  W-SNGP       PIC  9(008).
             03  W-ENGP       PIC  9(008).
           02  W-DMM          PIC  9(001).
           02  W-KMK.
             03  W-KMK1       PIC  9(004).
             03  W-KMK2       PIC  9(004).
           02  CNT            PIC  9(002).
           COPY  KANGEL.
           COPY  ACCUNT.
           COPY  FCTL.
      *       FD  SDH
       01  SDH_PRG410.
           02  SDH_PNAME1          PIC  X(009)  VALUE "SIWAKE-H1".
           02  F                   PIC  X(001).
           02  SDH_LNAME           PIC  X(003)  VALUE "SDH".
           02  F                   PIC  X(001).
           02  SDH_KEY1            PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2            PIC  X(100)  VALUE SPACE.
           02  SDH_SORT            PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST           PIC  X(100)  VALUE SPACE.
           02  SDH_RES             USAGE  POINTER.
           COPY  SIWAKH.
       77  F                       PIC  X(001).
      *       FD  SZF
       01  SZF_PRG410.
           02  SZF_PNAME1          PIC  X(009)  VALUE  SPACE.
           02  F                   PIC  X(001).
           02  SZF_LNAME           PIC  X(010)  VALUE "SZF_PRG410".
           02  F                   PIC  X(001).
           02  SZF_KEY1            PIC  X(100)  VALUE SPACE.
           02  SZF_KEY2            PIC  X(100)  VALUE SPACE.
           02  SZF_SORT            PIC  X(100)  VALUE SPACE.
           02  SZF_IDLST           PIC  X(100)  VALUE SPACE.
           02  SZF_RES             USAGE  POINTER.
       01  SZ-R.
           02  SZ-KEY.
             03  SZ-KMK      PIC  9(004).
             03  SZ-HOJ      PIC  9(004).
           02  SZ-TUKI.
             03  SZ-TUKID    OCCURS  12.
               04  SZ-NKIN   PIC S9(009).
               04  SZ-NSHZ   PIC S9(008).
               04  SZ-OKIN   PIC S9(008).
               04  SZ-OSHZ   PIC S9(007).
               04  SZ-HKIN   PIC S9(009).
           02  SZ-TSC        PIC  9(001).
           02  F             PIC  X(011).
       77  F                 PIC  X(001).
      *       
       77  USER_ID           PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE   PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER     PIC  9(003).
       77  ESTAT             PIC  X(002).
       77  RESU              PIC  9(001).
       77  RESP              PIC  9(001).
       77  RET               PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　月別消費税内訳ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(015) VALUE
                "【　'  年度  】".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-NEN   PIC  9(002).  
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  E-ME1.
             03  FILLER  PIC  X(032) VALUE
                  "***  ｺﾝﾄﾛｰﾙﾌｧｲﾙ ﾅｼ (      )  ***".
             03  FILLER  PIC  X(006).
           02  E-ME2.
             03  FILLER  PIC  X(036) VALUE
                  "***  ｶﾝｼﾞｶﾓｸﾏｽﾀ- ﾅｼ  (        )  ***".
             03  FILLER  PIC  X(008).
           02  E-ME3.
             03  FILLER  PIC  X(028) VALUE
                  "***  ｶﾓｸﾏｽﾀ- ﾅｼ  (    )  ***".
             03  FILLER  PIC  X(004).
           02  E-ME98  PIC  X(005) VALUE X"1B4A05".
           02  E-ME99  PIC  X(005) VALUE X"1B4205".
      *
       PROCEDURE      DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *       01  C-CLEAR.
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR"  RETURNING RESU.
      * 01  C-MID.
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "359" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "31" "15" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "29" "22" "08C-MID" " " RETURNING RESU.
      *       01  C-DSP.
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NEN" "9" "15" "36" "2" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
      *       01  C-ACP.
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "46" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *       01  C-ERR   LINE  24.
       CALL "SD_Init" USING 
            "C-ERR" " " "24" "0" "124" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" " " "24" "0" "38" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME1" "X" "24" "15" "32" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME1" "X" "24" "35" "6" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME1" BY REFERENCE FCTL-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" " " "24" "0" "44" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME2" "X" "24" "15" "36" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME2" "X" "24" "37" "8" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME2" BY REFERENCE KNG-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" " " "24" "0" "32" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME3" "X" "24" "15" "28" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME3" "X" "24" "33" "4" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME3" BY REFERENCE AM-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  " TO FCTL-KEY.
      *           READ FCTL-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE FCTL-UPDYY TO W-NEN.
           COMPUTE W-GET = FCTL-KSMM + 1.
           IF  W-GET = 13
               MOVE 1 TO W-GET
           END-IF.
           MOVE FCTL-TOUF(W-GET) TO W-SNGP.
           MOVE FCTL-TOUT(FCTL-KSMM) TO W-ENGP.
      *
           MOVE "TAX   " TO FCTL-KEY.
      *           READ FCTL-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE TAX-CODE TO W-KMK1.
           MOVE TAX-CODE1 TO W-KMK2.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NEN" D-NEN "p"
                                         RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING
           BY REFERENCE A-DMM "A-DMM" "9" "1"
           BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-10
           END-IF.
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO SZF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY3" BY REFERENCE SH-KEY3.
           CALL "DB_F_Open" USING
            "OUTPUT" SZF_PNAME1 " " BY REFERENCE SZF_IDLST "0".
       M-15.
      *           READ SDH NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDH_PNAME1 BY REFERENCE SH-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
           IF  HTRDATE  < W-SNGP OR > W-ENGP
               GO TO M-15
           END-IF.
           IF  HACCNTCD = W-KMK1 OR W-KMK2
               GO TO M-15
           END-IF.
           IF  HTAXKB NOT = " "
               GO TO M-20
           END-IF.
           IF  HETAX NOT = " "
               GO TO M-20
           END-IF.
           MOVE HKACD1 TO KNG-KEY.
      *           READ KNG UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           IF  KNGTAX = " "
               GO TO M-15
           END-IF.
       M-20.
           MOVE ZERO TO SZ-R.
           MOVE HKACD1 TO SZ-KEY.
           MOVE SZ-KMK TO AM-KEY.
      *           READ AM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           MOVE DR-CR TO SZ-TSC.
       M-25.
           MOVE HTRDATE  TO W-NGP.
           IF  W-GET < 5
               ADD 12 TO W-GET
           END-IF.
           COMPUTE CNT = W-GET - 4.
           IF  HTAXKB = "3" OR "7"
               IF  HDR-CR = DR-CR
                   ADD HAMOUNT TO SZ-NKIN(CNT)
               ELSE
                   SUBTRACT HAMOUNT FROM SZ-NKIN(CNT)
               END-IF
           END-IF.
           IF  HTAXKB = "1" OR "5"
               IF  HDR-CR = DR-CR
                   ADD HAMOUNT TO SZ-OKIN(CNT)
               ELSE
                   SUBTRACT HAMOUNT FROM SZ-OKIN(CNT)
               END-IF
           END-IF.
           IF  HTAXKB NOT = " "
               GO TO M-30
           END-IF.
           IF  HETAX = " "
               IF  HDR-CR = DR-CR
                   ADD HAMOUNT TO SZ-HKIN(CNT)
               ELSE
                   SUBTRACT HAMOUNT FROM SZ-HKIN(CNT)
               END-IF
           END-IF.
           IF  HETAX = "3" OR "7"
               IF  HDR-CR = DR-CR
                   ADD HAMOUNT TO SZ-NKIN(CNT)
                   SUBTRACT HAMOUNT FROM SZ-NSHZ(CNT)
               ELSE
                   SUBTRACT HAMOUNT FROM SZ-NKIN(CNT)
                   ADD HAMOUNT TO SZ-NSHZ(CNT)
               END-IF
           END-IF.
           IF  HETAX = "1" OR "5"
               IF  HDR-CR = DR-CR
                   ADD HAMOUNT TO SZ-OKIN(CNT)
                   SUBTRACT HAMOUNT FROM SZ-OSHZ(CNT)
               ELSE
                   SUBTRACT HAMOUNT FROM SZ-OKIN(CNT)
                   ADD HAMOUNT TO SZ-OSHZ(CNT)
               END-IF
            END-IF.
       M-30.
      *           READ SDH NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDH_PNAME1 BY REFERENCE SH-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF.
           IF  HTRDATE  < W-SNGP OR > W-ENGP
               GO TO M-30
           END-IF.
           IF  HACCNTCD = W-KMK1 OR W-KMK2
               GO TO M-30
           END-IF.
           IF  HTAXKB NOT = " "
               GO TO M-35
           END-IF.
           IF  HETAX NOT = " "
               GO TO M-35
           END-IF.
           MOVE HKACD1 TO KNG-KEY.
      *           READ KNG UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           IF  KNGTAX = " "
               GO TO M-30
           END-IF.
       M-35.
           IF  HKACD1 = SZ-KEY
               GO TO M-25
           END-IF.
      *           WRITE SZ-R.
      *///////////////
           CALL "DB_Insert" USING
            SZF_PNAME1 SZF_LNAME SZ-R RETURNING RET.
           GO TO M-20.
       M-80.
      *           WRITE SZ-R.
      *///////////////
           CALL "DB_Insert" USING
            SZF_PNAME1 SZF_LNAME SZ-R RETURNING RET.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SZF_IDLST SZF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

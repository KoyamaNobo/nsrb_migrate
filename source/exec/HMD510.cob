       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD510.
      *********************************************************
      *    PROGRAM         :  履物日次　在庫・売上更新        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/16                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ZERO-SW            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  W-INV              PIC  9(001) VALUE 0.
       01  W-DATA.
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-D.
             03  W-HCD        PIC  9(006).
             03  W-NS         PIC S9(007).
             03  W-NK         PIC S9(010).
             03  W-US         PIC S9(008).
             03  W-UK         PIC S9(010).
             03  W-YS         PIC S9(006).
             03  W-YK         PIC S9(009).
             03  W-UG         PIC S9(010).
             03  W-FT         PIC  9(005).
           02  CHK            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-FM           PIC  N(002).
           02  W-KIN          PIC S9(009).
           02  CNT            PIC  9(010).
           02  WD-D.
             03  WD-SUD.
               04  WD-SU      PIC S9(004)  OCCURS 10.
             03  WD-SUT       PIC S9(005).
             03  WD-UKI       PIC S9(008).
             03  WD-SKI       PIC S9(008).
           02  W-KEY.
             03  W-KEYD.
               04  W-HCDD     PIC  9(006).
               04  W-SIZD     PIC  9(001).
       01  W-AREA.
           02  INV-SW         PIC  9(001).
           02  I              PIC  9(002).
           02  W-SKC          PIC  9(001).
           02  W-SIZ          PIC  9(001).
      *
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LWMSG.
      *
           COPY LIHIM.
           COPY LIHHTF.
           COPY LIHUHM.
           COPY LUTRAN.
      *FD  STRAN
       01  STRAN_HMD510.
           02  STRAN_PNAME1   PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  STRAN_LNAME    PIC  X(012) VALUE "STRAN_HMD510".
           02  F              PIC  X(001).
           02  STRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  STRAN_KEY2     PIC  X(100) VALUE SPACE.
           02  STRAN_SORT     PIC  X(100) VALUE SPACE.
           02  STRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  STRAN_RES      USAGE  POINTER.
       01  STRAN-R.
           02  S-DNO          PIC  9(006).
           02  S-GNO          PIC  9(001).
           02  S-DATE.
             03  S-NG         PIC  9(006).
             03  F            PIC  9(002).
           02  S-TCD          PIC  9(004).
           02  S-KEY.
             03  S-HCD        PIC  9(006).
             03  S-SIZ        PIC  9(001).
           02  S-SUD.
             03  S-SU    OCCURS  10  PIC S9(004)  COMP-3.
           02  S-SUT          PIC S9(005).
           02  S-T            PIC  9(005).
           02  S-UKI          PIC S9(008).
           02  F              PIC  X(001).
           02  S-DK           PIC  9(001).
           02  S-FT           PIC  9(005).
           02  F              PIC  X(003).
           02  S-BC.
             03  S-BC1        PIC  9(002).
             03  S-BC2        PIC  9(002).
             03  S-BC3        PIC  9(002).
           02  S-SKC          PIC  9(001).
           02  F              PIC  X(023).
           02  S-SNGP         PIC  9(008).
           02  F              PIC  X(004).
           02  S-DHC          PIC  9(001).
           02  S-UNC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  TAZ-M
       01  TAZ-M_HMD510.
           02  TAZ-M_PNAME1   PIC  X(004) VALUE "TAZM".
           02  F              PIC  X(001).
           02  TAZ-M_LNAME    PIC  X(012) VALUE "TAZ-M_HMD510".
           02  F              PIC  X(001).
           02  TAZ-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TAZ-M_KEY2     PIC  X(100) VALUE SPACE.
           02  TAZ-M_SORT     PIC  X(100) VALUE SPACE.
           02  TAZ-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TAZ-M_RES      USAGE  POINTER.
       01  TAZ-R.
           02  TAZ-KEY.
             03  TAZ-TCD      PIC  9(004).
             03  TAZ-HCD      PIC  9(006).
           02  TAZ-AZS        PIC S9(005).
           02  TAZ-AAS        PIC S9(005).
           02  TAZ-SZS        PIC S9(005).
           02  TAZ-SAS        PIC S9(005).
           02  F              PIC  X(006).
           02  TAZ-NG         PIC  9(006).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物日次　在庫・売上更新　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-ED.
               04  01D-ED  PIC  N(002).
               04  02D-ED  PIC  9(006).
               04  FILLER  PIC  N(014) VALUE
                    "ハードコピーをし，ＲＥＳＥＴ".
             03  D-EDC   PIC  X(050) VALUE
                  "                                                  ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(030) VALUE
                  "***  売上，値引伝票未発行  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  TAZM ﾅｼ  ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  TAZM WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  TAZM REWRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(017) VALUE
                  "***  HUHM ﾅｼ  ***".
             03  E-ME10  PIC  X(026) VALUE
                  "***  HUHM REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME12  PIC  X(032) VALUE
                  "***  STRAN･HIM ﾌﾘｶｴﾀﾝｶ ﾌｲｯﾁ  ***".
             03  E-ME21  PIC  X(025) VALUE
                  "***  HHTF ﾅｼ (UTRAN)  ***".
             03  E-ME22  PIC  X(034) VALUE
                  "***  HHTF REWRITE ｴﾗｰ (UTRAN)  ***".
             03  E-ME23  PIC  X(024) VALUE
                  "***  HHTF ﾅｼ (SYUF)  ***".
             03  E-ME27  PIC  X(033) VALUE
                  "***  HHTF REWRITE ｴﾗｰ (SYUF)  ***".
             03  E-KEY1  PIC  9(007).
             03  E-KEY2  PIC  9(007).
             03  E-KEY3  PIC  X(010).
             03  E-KEY4  PIC  9(007).
             03  E-KEY9  PIC  9(006).
           COPY LSSEM.
           COPY LSMSG.
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
            "C-MID" " " "0" "0" "330" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "30" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "47" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *D-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "88" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "18" "0" "88" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ED" " " "18" "0" "38" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ED" "N" "18" "12" "4" " " "D-ED"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-ED" BY REFERENCE W-FM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ED" "9" "18" "17" "6" "01D-ED" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-ED" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ED" "N" "18" "25" "28" "02D-ED" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EDC" "X" "18" "10" "50" "D-ED" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "341" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "341" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "30" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "24" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "17" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "26" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "16" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "32" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "25" "E-ME12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "34" "E-ME21" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME23" "X" "24" "15" "24" "E-ME22" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME27" "X" "24" "15" "33" "E-ME23" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY1" "9" "24" "60" "7" "E-ME27" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY1" BY REFERENCE W-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY2" "9" "24" "60" "7" "E-KEY1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY2" BY REFERENCE S-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY3" "X" "24" "60" "10" "E-KEY2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY3" BY REFERENCE TAZ-KEY "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY4" "9" "24" "60" "7" "E-KEY3" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY4" BY REFERENCE W-KEYD "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY9" "9" "24" "60" "6" "E-KEY4" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY9" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-030.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-030
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-030
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" STRAN_PNAME1 " " BY REFERENCE STRAN_IDLST "0".
       M-040.
      *           READ STRAN AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-060
           END-IF
           IF  S-DHC NOT = 0
               GO TO M-040
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       M-060.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" STRAN_PNAME1 " " BY REFERENCE STRAN_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "DB_F_Open" USING
            "I-O" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TAZ-M_PNAME1 "SHARED" BY REFERENCE TAZ-M_IDLST "1"
            "TAZ-KEY" BY REFERENCE TAZ-KEY.
      *----　仕上・受入　-----------------------------------------------
           MOVE 1 TO CHK.
       M-100.
      *           READ UTRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-300
           END-IF
           IF  UTRAN-NRC = 2
               GO TO M-100
           END-IF.
       M-120.
           MOVE ZERO TO W-D W-DC.
           MOVE UTRAN-HCD TO W-HCD.
       M-140.
           IF  UTRAN-NRC = 5
               SUBTRACT UTRAN-SUT FROM W-NS W-YS
               SUBTRACT UTRAN-FKIN FROM W-NK W-YK
           ELSE
               ADD UTRAN-SUT TO W-NS W-YS
               ADD UTRAN-FKIN TO W-NK W-YK
           END-IF
           MOVE UTRAN-HCD TO W-HCDD.
           MOVE UTRAN-SIZ TO W-SIZD.
      *
           MOVE 0 TO W-INV.
           MOVE W-KEYD TO HHT-KEY.
      *           READ HHTF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY4" E-KEY4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE ZERO  TO CNT.
       M-160.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO M-180
           END-IF
           IF  UTRAN-SU(CNT) = ZERO
               GO TO M-160
           END-IF
           IF  W-INV = 0
               IF  UTRAN-NRC = 5
                   SUBTRACT UTRAN-SU(CNT) FROM HHT-NSU(CNT)
               ELSE
                   ADD UTRAN-SU(CNT) TO HHT-NSU(CNT)
               END-IF
           END-IF
           GO TO M-160.
       M-180.
           IF W-INV NOT = 0
               GO TO M-200
           END-IF
      *           REWRITE HHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY4" E-KEY4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-980
           END-IF.
       M-200.
      *           READ UTRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-220
           END-IF
           IF UTRAN-NRC = 2
               GO TO M-200
           END-IF
           IF W-HCD = UTRAN-HCD
               GO TO M-140
           END-IF
           PERFORM HUH-RTN THRU HUH-EX.
           GO TO M-120.
       M-220.
           PERFORM HUH-RTN THRU HUH-EX.
      *----　出　荷　---------------------------------------------------
       M-300.
           MOVE ZERO TO CHK.
           MOVE 2 TO CHK.
      *           READ STRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-980
           END-IF
           IF  S-GNO = 9
               GO TO M-300
           END-IF
           IF  S-HCD = ZERO
               GO TO M-300
           END-IF
           IF  S-UNC = 0
               IF  S-DK = 6 OR 8
                   GO TO M-300
               END-IF
           END-IF.
       M-320.
           MOVE ZERO TO W-D.
           MOVE S-HCD TO W-HCD.
       M-340.
           IF  S-UNC = 1
               SUBTRACT S-UKI FROM W-UK
               GO TO M-400
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-FT
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY9" E-KEY9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  S-DK = 0 OR 1 OR 3 OR 5
               IF  S-FT NOT = HI-FT
                   CALL "SD_Output" USING
                    "E-ME12" E-ME12 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY9" E-KEY9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           PERFORM UHS-RTN THRU UHS-EX.
           IF  S-DK = 2
               GO TO M-400
           END-IF
           IF  S-DK = 3 OR 4 OR 9
               PERFORM TZM-RTN THRU TZM-EX
           END-IF
           IF  S-HCD > 999899
               GO TO M-400
           END-IF
      *
           MOVE 0 TO W-INV.
           MOVE S-HCD TO HHT-HCD.
           MOVE S-SIZ TO HHT-SIZ.
      *           READ HHTF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HHTF_PNAME1 BY REFERENCE HHT-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY2" E-KEY2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
      *
           MOVE ZERO TO CNT.
       M-360.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO M-380
           END-IF
           IF  WD-SU(CNT) = ZERO
               GO TO M-360
           END-IF
           IF  W-INV = 0
               IF  S-DK = 4
                   ADD WD-SU(CNT) TO HHT-ASS(CNT)
               ELSE
                   ADD WD-SU(CNT) TO HHT-USU(CNT)
               END-IF
           END-IF
           GO TO M-360.
       M-380.
      *           REWRITE HHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HHTF_PNAME1 HHTF_LNAME HHT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME27" E-ME27 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY2" E-KEY2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF.
       M-400.
      *           READ STRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" STRAN_PNAME1 BY REFERENCE STRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-420
           END-IF
           IF  S-GNO = 9
               GO TO M-400
           END-IF
           IF  S-HCD = ZERO
               GO TO M-400
           END-IF
           IF  S-UNC = 0
               IF  S-DK = 6 OR 8
                   GO TO M-400
               END-IF
           END-IF
           IF  W-HCD = S-HCD
               GO TO M-340
           END-IF
           PERFORM HUH-RTN THRU HUH-EX.
           GO TO M-320.
       M-420.
           PERFORM HUH-RTN THRU HUH-EX.
       M-980.
           PERFORM  END-RTN  THRU  END-EX.
           CALL "DB_Close".
           STOP RUN.
       HUH-RTN.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-FT
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY9" E-KEY9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE W-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY9" E-KEY9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HUH-EX
           END-IF
           COMPUTE W-KIN = HUH-YK + W-YK.
           ADD W-NS TO HUH-NS.
           ADD W-NK TO HUH-NK.
           ADD W-US TO HUH-SS.
           ADD W-UK TO HUH-SK.
           ADD W-YS TO HUH-YS.
           COMPUTE HUH-YK = HUH-YS * HI-FT.
           ADD W-UG TO HUH-UG.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  HUH-YK = W-KIN
               GO TO HUH-EX
           END-IF
           IF  W-DC NOT = ZERO
               GO TO HUH-EX
           END-IF
           MOVE 5 TO W-DC.
           IF  CHK = 1
               MOVE "入庫" TO W-FM
           END-IF
           IF  CHK = 2
               MOVE "売上" TO W-FM
           END-IF
           CALL "SD_Output" USING "D-ED" D-ED "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-EDC" D-EDC "p" RETURNING RESU.
       HUH-EX.
           EXIT.
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STRAN_IDLST STRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
       DST-RTN.
           MOVE ZERO TO WD-D.
           IF (S-HCD > 999899) OR (S-DK = 3)
               MOVE ZERO TO S-SU(01) S-SU(02) S-SU(03) S-SU(04)
                 S-SU(05) S-SU(06) S-SU(07) S-SU(08) S-SU(09) S-SU(10)
           END-IF
           IF  S-DK = 0 OR 3 OR 4 OR 7 OR 9
               MOVE S-SU(01) TO WD-SU(01)
               MOVE S-SU(02) TO WD-SU(02)
               MOVE S-SU(03) TO WD-SU(03)
               MOVE S-SU(04) TO WD-SU(04)
               MOVE S-SU(05) TO WD-SU(05)
               MOVE S-SU(06) TO WD-SU(06)
               MOVE S-SU(07) TO WD-SU(07)
               MOVE S-SU(08) TO WD-SU(08)
               MOVE S-SU(09) TO WD-SU(09)
               MOVE S-SU(10) TO WD-SU(10)
               MOVE S-SUT TO WD-SUT
               MOVE S-UKI TO WD-UKI
               COMPUTE WD-SKI = S-SUT * S-FT
           ELSE
               COMPUTE WD-SU(01) = -1 * S-SU(01)
               COMPUTE WD-SU(02) = -1 * S-SU(02)
               COMPUTE WD-SU(03) = -1 * S-SU(03)
               COMPUTE WD-SU(04) = -1 * S-SU(04)
               COMPUTE WD-SU(05) = -1 * S-SU(05)
               COMPUTE WD-SU(06) = -1 * S-SU(06)
               COMPUTE WD-SU(07) = -1 * S-SU(07)
               COMPUTE WD-SU(08) = -1 * S-SU(08)
               COMPUTE WD-SU(09) = -1 * S-SU(09)
               COMPUTE WD-SU(10) = -1 * S-SU(10)
               COMPUTE WD-SUT = -1 * S-SUT
               COMPUTE WD-UKI = -1 * S-UKI
               COMPUTE WD-SKI = WD-SUT * S-FT
           END-IF
           IF  S-DK = 3
               MOVE 4 TO S-SIZ
               MOVE WD-SUT TO WD-SU(10)
           END-IF
           IF  S-HCD > 999899
               MOVE ZERO TO WD-SUD WD-SUT
           END-IF.
       DST-EX.
           EXIT.
       UHS-RTN.
           IF  S-DK NOT = 4
               IF  S-DK = 2
                   ADD WD-UKI TO W-UK
                   ADD WD-SKI TO W-NK
               ELSE
                   ADD WD-SUT TO W-US
                   ADD WD-UKI TO W-UK
                   SUBTRACT WD-SUT FROM W-YS
                   SUBTRACT WD-SKI FROM W-YK
                   ADD WD-SKI TO W-UG
               END-IF
           END-IF.
       UHS-EX.
           EXIT.
       TZM-RTN.
           MOVE 0 TO W-ACT.
           MOVE S-TCD TO TAZ-TCD.
           MOVE S-HCD TO TAZ-HCD.
      *           READ TAZ-M INVALID KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" TAZ-M_PNAME1 BY REFERENCE TAZ-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-ACT
               GO TO TZM-020
           END-IF
           MOVE 2 TO W-ACT.
           GO TO TZM-040.
       TZM-020.
           MOVE ZERO TO TAZ-R
           MOVE S-TCD TO TAZ-TCD.
           MOVE S-HCD TO TAZ-HCD.
           MOVE S-NG TO TAZ-NG.
       TZM-040.
           IF  S-DK = 3 OR 9
               ADD S-SUT TO TAZ-AAS
               IF  S-SNGP NOT = 99999999
                   ADD S-SUT TO TAZ-SAS
               END-IF
           END-IF
           IF  S-DK = 4
               SUBTRACT S-SUT FROM TAZ-AAS TAZ-SAS
           END-IF
           IF  W-ACT NOT = 1
               GO TO TZM-080
           END-IF
      *           WRITE TAZ-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TAZ-M_PNAME1 TAZ-M_LNAME TAZ-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY3" E-KEY3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TZM-060
           END-IF
           GO TO TZM-EX.
       TZM-060.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO TZM-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TAZ-M_IDLST TAZ-M_PNAME1.
           MOVE "TAZM         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TAZ-M_PNAME1 "SHARED" BY REFERENCE TAZ-M_IDLST "1"
            "TAZ-KEY" BY REFERENCE TAZ-KEY.
           GO TO TZM-020.
       TZM-080.
      *           REWRITE TAZ-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TAZ-M_PNAME1 TAZ-M_LNAME TAZ-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"         TO  ERR-M
               MOVE  "TAZM"      TO  ERR-F
               MOVE  TAZ-KEY     TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       TZM-EX.
           EXIT.
           COPY  LPMSG.

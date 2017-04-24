       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD120.
      *********************************************************
      *    PROGRAM         :  購買印字・工品未変換チェック    *
      *    JS-SIGN         :  購買･工品=0 , 工品=1            *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-KSU.
             03  W-JSP        PIC  9(003).
             03  W-HAP        PIC  9(003).
             03  W-HNK        PIC  9(003).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *FD  JSS-F
       01  JSS-F_KBD120.
           02  JSS-F_PNAME1   PIC  X(004)  VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012)  VALUE "JSS-F_KBD120".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  JSS-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100)  VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC          PIC  9(002).
           02  JS-DATE        PIC  9(008).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SU          PIC S9(007)V9(02).
           02  JS-T           PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  JS-SHZ         PIC S9(007).
           02  JS-CD          PIC  9(006).
           02  JS-SJCD        PIC  9(006).
           02  JS-NNO         PIC  9(006).
           02  JS-FC          PIC  9(001).
           02  JS-YC          PIC  9(001).
           02  JS-TC          PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  JS-BSC         PIC  9(001).
           02  JS-BKC         PIC  9(002).
           02  JS-KCO         PIC  X(005).
           02  JS-KHC         PIC  9(001).
           02  F              PIC  X(010).
           02  JS-KEY.
             03  JS-DNO       PIC  X(006).
             03  JS-GNO       PIC  9(001).
           02  JS-PCNT        PIC  9(001).
       77  F                  PIC  X(001).
      *FD  HA-F
       01  HA-F_KBD120.
           02  HA-F_PNAME1   PIC  X(003)  VALUE "HAF".
           02  F             PIC  X(001).
           02  HA-F_LNAME    PIC  X(011)  VALUE "HA-F_KBD120".
           02  F             PIC  X(001).
           02  HA-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  HA-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  HA-F_SORT     PIC  X(100)  VALUE SPACE.
           02  HA-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  HA-F_RES      USAGE  POINTER.
       01  HA-R.
           02  F              PIC  9(002).
           02  HA-DATE        PIC  9(006).
           02  HA-JCD         PIC  9(006).
           02  HA-SSU         PIC S9(007)V9(02).
           02  HA-KEY.
             03  HA-DNO       PIC  9(006).
             03  HA-GNO       PIC  9(001).
           02  F              PIC  X(001).
           02  HA-PCNT        PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER   PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER   PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER   PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER   PIC  N(025) VALUE
                "＊＊＊　　購買未印字・工品未変換チェック　　＊＊＊".
           02  FILLER   PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER   PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER   PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER   PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM      PIC  9(001).
       01  C-DSP.
           02  D-MID1      PIC  N(025) VALUE
                "＊＊＊　　　　　工品未変換チェック　　　　　＊＊＊".
           02  D-JSP.
             03  FILLER  PIC  N(008) VALUE "仕入未印字データ".
             03  FILLER  PIC ZZ9.
             03  FILLER  PIC  N(001) VALUE "件".
           02  D-HAP.
             03  FILLER  PIC  N(008) VALUE "出庫未印字データ".
             03  FILLER  PIC ZZ9.
             03  FILLER  PIC  N(001) VALUE "件".
           02  D-HNK.
             03  FILLER  PIC  N(008) VALUE "工品未変換データ".
             03  FILLER  PIC ZZ9.
             03  FILLER  PIC  N(001) VALUE "件".
       01  C-ERR.
           02  FILLER.
             03  E-STAT     PIC  X(002).
             03  E-ME98     PIC  X(005) VALUE X"1B4A05".
             03  E-ME99     PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER   PIC  X(040) VALUE
                    "                                        ".
               04  FILLER   PIC  X(040) VALUE
                    "                                        ".
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "372" " " " " RETURNING RESU.
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
            "08C-MID" "X" "22" "30" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "47" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "113" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID1" "N" "6" "10" "50" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-JSP" " " "14" "0" "21" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-JSP" "N" "14" "23" "16" " " "D-JSP" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-JSP" "ZZ9" "14" "41" "3" "01D-JSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-JSP" BY REFERENCE W-JSP "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-JSP" "N" "14" "45" "2" "02D-JSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-HAP" " " "16" "0" "21" "D-JSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-HAP" "N" "16" "23" "16" " " "D-HAP" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-HAP" "ZZ9" "16" "41" "3" "01D-HAP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-HAP" BY REFERENCE W-HAP "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-HAP" "N" "16" "45" "2" "02D-HAP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-HNK" " " "18" "0" "21" "D-HAP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-HNK" "N" "18" "23" "16" " " "D-HNK" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-HNK" "ZZ9" "18" "41" "3" "01D-HNK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-HNK" BY REFERENCE W-HNK "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-HNK" "N" "18" "45" "2" "02D-HNK" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "92" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "92" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
	           CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                  RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" 
                                  RETURNING RESU
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           IF  W-DMM = 9
               GO TO M-95
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                  RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" 
                                  RETURNING RESU
           END-IF.
           MOVE ZERO TO W-KSU.
           CALL "DB_F_Open" USING
            "INPUT" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
       M-15.
      *           READ JSS-F NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF.
           IF  JS-SIGN = 1
               IF  JS-PCNT = 0
                   ADD 1 TO W-JSP
               END-IF
           END-IF.
           IF  JS-KCO = ZERO OR SPACE
               GO TO M-15
           END-IF.
           IF  JS-JCD < 490000 OR > 498999
               GO TO M-15
           END-IF.
           IF  JS-DC NOT = 10
               GO TO M-15
           END-IF.
           IF  JS-KHC NOT = 2
               ADD 1 TO W-HNK
           END-IF.
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           IF  JS-SIGN = 1
               GO TO M-35
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" HA-F_PNAME1 "SHARED" BY REFERENCE HA-F_IDLST "1"
            "HA-KEY" BY REFERENCE HA-KEY.
       M-25.
      *           READ HA-F NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HA-F_PNAME1 BY REFERENCE HA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF.
           IF  HA-PCNT = 0
               ADD 1 TO W-HAP
           END-IF.
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING BY REFERENCE HA-F_IDLST HA-F_PNAME1.
       M-35.
           IF  JS-SIGN = 0
               IF  W-JSP NOT = ZERO
                   CALL "SD_Output" USING "D-JSP" D-JSP "p" 
                                  RETURNING RESU
               END-IF
           END-IF.
           IF  JS-SIGN = 0
               IF  W-HAP NOT = ZERO
                   CALL "SD_Output" USING "D-HAP" D-HAP "p" 
                                  RETURNING RESU
               END-IF
           END-IF.
           IF  W-HNK NOT = ZERO
               CALL "SD_Output" USING "D-HNK" D-HNK "p" 
                                  RETURNING RESU
           END-IF.
           IF  JS-SIGN = 0
               IF  ZERO = W-JSP AND W-HAP AND W-HNK
                   GO TO M-95
               END-IF
           END-IF.
           IF  JS-SIGN = 1
               IF  ZERO = W-HNK
                   GO TO M-95
               END-IF
           END-IF.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 255.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
	       CALL "DB_Close".
           STOP RUN.

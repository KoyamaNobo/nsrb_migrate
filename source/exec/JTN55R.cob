       IDENTIFICATION DIVISION.
       PROGRAM-ID. JTN55R.
      *********************************************************
      *    PROGRAM         :  送り状送り先明細問合せ          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-MSG              PIC  X(040).
       77  W-FILE             PIC  X(013) VALUE SPACE.
       01  W-DATA.
           02  W-ONO          PIC  9(006).
           02  W-NGP          PIC  9(008).
           02  W-NGPL  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  F            PIC  9(002).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NG         PIC  9(006).
             03  W-PEY        PIC  9(002).
           02  W-INV          PIC  9(001).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LITCM.
           COPY L-JCON.
      *FD  OSMF
       01  OSMF_JTN55R.
           02  OSMF_PNAME1    PIC  X(008) VALUE "OSMF-RDB".
           02  F              PIC  X(001).
           02  OSMF_LNAME     PIC  X(011) VALUE "OSMF_JTN55R".
           02  F              PIC  X(001).
           02  OSMF_KEY1      PIC  X(100) VALUE SPACE.
           02  OSMF_SORT      PIC  X(100) VALUE SPACE.
           02  OSMF_IDLST     PIC  X(100) VALUE SPACE.
           02  OSMF_RES       USAGE  POINTER.
       01  OSM-R.
           02  OSM-ONO        PIC  9(006).
           02  OSM-NGP        PIC  9(008).
           02  OSM-NGPD  REDEFINES OSM-NGP.
             03  OSM-NG       PIC  9(006).
             03  OSM-PEY      PIC  9(002).
           02  OSM-OSC.
             03  OSM-TCD      PIC  9(004).
             03  OSM-CCD      PIC  9(003).
           02  OSM-KUR        PIC  9(001).
           02  OSM-UNS        PIC  9(001).
           02  OSM-KSU        PIC  9(003).
           02  F              PIC  X(002).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　送り状　送り先明細　問合せ　　＊＊＊".
           02  FILLER  PIC  N(004) VALUE
                "送り状№".
           02  FILLER  PIC  X(007) VALUE
                  "終了=F9".
           02  FILLER  PIC  X(026) VALUE
                  "日    付   '  年   月   日".
           02  FILLER  PIC  N(004) VALUE
                "直送先名".
           02  FILLER  PIC  N(004) VALUE
                "得意先名".
           02  FILLER  PIC  N(004) VALUE
                "倉　　庫".
           02  FILLER  PIC  N(004) VALUE
                "運送会社".
           02  FILLER  PIC  N(004) VALUE
                "個　　数".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-ONO   PIC  9(006).
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-PEY   PIC  9(002).
           02  D-CNA.
             03  FILLER  PIC  9(003).
             03  FILLER  PIC  N(026).
           02  D-TNA.
             03  FILLER  PIC  9(004).
             03  FILLER  PIC  N(026).
           02  D-SOK.
             03  FILLER  PIC  9(001).
             03  FILLER  PIC  N(006).
           02  D-UNS.
             03  FILLER  PIC  9(001).
             03  FILLER  PIC  N(006).
           02  D-KSU   PIC ZZZ,ZZ9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "149" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "6" "8" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "4" "30" "7" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "6" "6" "26" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "9" "6" "8" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "11" "6" "8" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "13" "6" "8" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "N" "15" "6" "8" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "N" "17" "6" "8" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "40" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ONO" "9" "4" "17" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ONO" BY REFERENCE W-ONO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "6" "0" "4" "A-ONO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "6" "18" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "6" "23" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "57" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "146" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEY" "9" "6" "28" "2" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CNA" " " "9" "0" "55" "D-PEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CNA" "9" "9" "17" "3" " " "D-CNA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-CNA" BY REFERENCE TC-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CNA" "N" "9" "22" "52" "01D-CNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-CNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" " " "11" "0" "56" "D-CNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TNA" "9" "11" "17" "4" " " "D-TNA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TNA" BY REFERENCE T-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TNA" "N" "11" "22" "52" "01D-TNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SOK" " " "13" "0" "13" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SOK" "9" "13" "17" "1" " " "D-SOK" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SOK" BY REFERENCE JCON3-02 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SOK" "N" "13" "22" "12" "01D-SOK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SOK" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UNS" " " "15" "0" "13" "D-SOK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-UNS" "9" "15" "17" "1" " " "D-UNS" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-UNS" BY REFERENCE JCON2-02 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-UNS" "N" "15" "22" "12" "01D-UNS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-UNS" BY REFERENCE JCON2-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KSU" "ZZZ,ZZ9" "17" "17" "7" "D-UNS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KSU" BY REFERENCE OSM-KSU "3" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "40" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ONO "A-ONO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ONO = ZERO
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-NEN2 < 16
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-20
           END-IF.
       M-25.
           MOVE 20 TO W-NEN1.
      *
           MOVE 0 TO W-INV.
           CALL "DB_F_Open" USING
            "INPUT" OSMF_PNAME1 " " BY REFERENCE OSMF_IDLST "0".
      *           SELECT OSMF WHERE OSM-ONO = W-ONO AND OSM-NG = W-NG.
           CALL "DB_Select" USING OSMF_PNAME1 "WHERE" 
            "OSM-ONO" "=" W-ONO "AND"
            "OSM-NG" "<=" W-NG RETURNING RET.
      *           READ OSMF NEXT RECORD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" OSMF_PNAME1 BY REFERENCE OSM-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING OSMF_PNAME1
               MOVE SPACE TO W-MSG
               MOVE "***  DATA ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               MOVE 1 TO W-INV
               GO TO M-30
           END-IF
      *
           MOVE OSM-NGP TO W-NGP.
           CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU.
      *
           MOVE OSM-OSC TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "　直送先なし" TO TC-NAME
           END-IF
      *
           MOVE OSM-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　得意先なし" TO T-NAME
           END-IF
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE 3 TO JCON3-01.
           MOVE OSM-KUR TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON1-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　倉庫なし　" TO JCON3-03
           END-IF
           CALL "SD_Output" USING "D-SOK" D-SOK "p" RETURNING RESU.
      *
           MOVE 2 TO JCON2-01.
           MOVE OSM-UNS TO JCON2-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON1-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　運送なし　" TO JCON2-03
           END-IF
           CALL "SD_Output" USING "D-UNS" D-UNS "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KSU" D-KSU "p" RETURNING RESU.
       M-30.
           CALL "DB_F_Close" USING BY REFERENCE OSMF_IDLST OSMF_PNAME1.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-INV = 1
               IF  ESTAT = BTB
                   GO TO M-15
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

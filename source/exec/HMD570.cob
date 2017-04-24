       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD570.
       AUTHOR. S-NAKAO.
       DATE-WRITTEN. 1974-05-01.
      *********************************************************
      *    PROGRAM         :  履物　在庫集計表　作成　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  99/01/30                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "実在庫数".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(003) VALUE "預り数".
       01  W-P.
           02  P-ZSK          PIC ---,---,---,--9.
           02  P-AZ           PIC ---,---,---,--9.
       01  W-EM.
           02  F              PIC  N(008) VALUE "［　日計更新終了".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  EM-DATE        PIC 99B99B99BBB.
           02  F              PIC  X(005)  VALUE "TIME ".
           02  EM-TIME        PIC 99B99B99.
           02  F              PIC  N(002)  VALUE "　］".
       01  W-DATA.
           02  CNT            PIC  9(002).
           02  W-NG           PIC  9(004).
           02  W-AD.
             03  W-ZSK        PIC S9(008).
             03  W-AZ         PIC S9(008).
           02  W-TIMED.
             03  W-TIME       PIC  9(006).
             03  F            PIC  X(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHHTF.
           COPY LSPF.
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　履物　在庫集計表　作成　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(029) VALUE
                "作表  する=5  しない=0   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "295" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "15" "29" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "15" "39" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ACCEPT EM-DATE FROM DATE.
           ACCEPT W-TIMED FROM TIME.
           MOVE W-TIME TO EM-TIME.
           CALL "PR_Open" RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-EM TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           COPY LIBCPR.
           IF  D-HKC NOT = 1
               GO TO M-95
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = ZERO
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 5
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO W-AD.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HHTF_IDLST HHTF_PNAME1
               GO TO M-95
           END-IF.
       M-15.
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-25
           END-IF
           IF (HHT-SIZ = 4) AND (CNT = 10)
               COMPUTE W-AZ = W-AZ + HHT-ZSU(10) - HHT-USU(10)
           ELSE
               COMPUTE W-AZ = W-AZ + HHT-ASS(CNT)
               COMPUTE W-ZSK = W-ZSK + HHT-ZSU(CNT) + HHT-NSU(CNT)
                                     - HHT-USU(CNT) - HHT-ASS(CNT)
           END-IF
           GO TO M-20.
       M-25.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           GO TO M-15.
       M-40.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
       M-90.
      *
           COMPUTE W-AZ = W-AZ * -1.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P.
           MOVE W-ZSK TO P-ZSK.
           MOVE W-AZ TO P-AZ.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

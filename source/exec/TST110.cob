       IDENTIFICATION DIVISION.
       PROGRAM-ID. TST110.
      **********************************************
      *****     取引先別　支払手形　問合せ     *****
      *****        ( SCREEN : SCTT11 )         *****
      **********************************************
       DATE-WRITTEN. 78-03-24.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　取引先別　支払手形　チェックリスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  H-TCD          PIC  9(004).
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "所在地".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-ADR          PIC  N(008).
           02  F              PIC  N(004) VALUE "取引先名".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-TNA          PIC  N(024).
       01  HEAD3.
           02  F              PIC  X(035) VALUE
                "       日　付　手形NO 種類   期　日".
           02  F              PIC  X(032) VALUE
                "       金　　額 落込　振出銀行名".
           02  F              PIC  X(023) VALUE SPACE.
       01  W-P.
           02  F              PIC  X(006).
           02  P-FRI          PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-KEY          PIC  9(004).
           02  P-TM           PIC  N(008).
           02  P-D     REDEFINES P-TM.
             03  F            PIC  X(002).
             03  P-KBN        PIC  N(002).
             03  F            PIC  X(002).
             03  P-MAN        PIC 99/99/99.
           02  P-KIN          PIC ZZ,ZZZ,ZZZ,ZZ9.
           02  F              PIC  X(003).
           02  P-OK           PIC  X(001).
           02  F              PIC  X(003).
           02  P-BKND1.
             03  P-BKN1       PIC  N(008).
             03  F            PIC  X(003).
           02  P-BKND2 REDEFINES P-BKND1.
             03  F            PIC  X(003).
             03  P-BKN2       PIC  N(008).
           02  F              PIC  X(014).
       01  W-DATA.
           02  W-FRI          PIC  9(006).
           02  W-FRID         PIC 99/99/99.
           02  W-FRIC  REDEFINES W-FRID  PIC  X(008).
           02  W-MAND         PIC 99/99/99.
           02  W-MANC  REDEFINES W-MAND  PIC  X(008).
           02  W-BK           PIC  N(008).
           02  W-BKND1.
             03  W-BKN1       PIC  N(008).
             03  F            PIC  N(001).
           02  W-BKND2 REDEFINES W-BKND1.
             03  F            PIC  N(001).
             03  W-BKN2       PIC  N(008).
           02  W-TCD          PIC  9(004).
           02  W-KIN          PIC  9(010) VALUE ZERO.
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-JUP.
             03  ADR1    OCCURS   8  PIC  N(001).
             03  F            PIC  N(016).
           02  W-ADR.
             03  ADR2    OCCURS   8  PIC  N(001).
           02  CNT            PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-KBN          PIC  X(001).
           02  W-OK           PIC  X(001).
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LIBANK.
           COPY LSSHIT.
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　取引先別　支払手形帳　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(035) VALUE
                "[   問合せ = 0  作表 = 5   ﾘﾀｰﾝ   ]".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-PTCD  PIC  9(004).
           02  A-TTCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-PM    PIC  X(036) VALUE
                "<   取引先ｺｰﾄﾞ        >     終了=PF9".
           02  D-PTNA  PIC  N(024).
           02  D-TNA   PIC  N(024).
           02  FILLER.
             03  D-D.
               04  FILLER  PIC  X(008).
               04  FILLER  PIC  X(004).
               04  FILLER  PIC  X(001).
               04  FILLER  PIC  X(008).
               04  FILLER  PIC ZZZZ,ZZZ,ZZ9 .
               04  FILLER  PIC  X(001).
               04  FILLER  PIC  N(009).
             03  D-TD.
               04  FILLER  PIC  X(008).
               04  FILLER  PIC ZZZZ,ZZZ,ZZ9 .
           02  FILLER.
             03  D-NM    PIC  X(038) VALUE
                  "[  NEXT PAGE = ﾘﾀｰﾝ  END = BSKIP     ]".
             03  D-EM    PIC  X(038) VALUE
                  "                     [  END DATA     ]".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "343" " " " "  RETURNING RESU.
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
            "08C-MID" "X" "12" "14" "35" "07C-MID" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "12" "40" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PTCD" "9" "15" "28" "4" "A-ACT" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PTCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TTCD" "9" "3" "16" "4" "A-PTCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TTCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "53" "1" "A-TTCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "280" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "15" "13" "36" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PTNA" "N" "17" "16" "48" "D-PM" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-PTNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "3" "21" "48" "D-PTNA" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L" "0" "72" "D-TNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D" " " "W-L" "13" "52" " " "04C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-D" "X" "W-L" "11" "8" " " "D-D"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-D" BY REFERENCE W-FRIC "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-D" "X" "W-L" "20" "4" "01D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-D" BY REFERENCE ST-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-D" "X" "W-L" "25" "1" "02D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "03D-D" BY REFERENCE W-KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-D" "X" "W-L" "27" "8" "03D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "04D-D" BY REFERENCE W-MANC "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-D" "ZZZZ,ZZZ,ZZ9" "W-L" "36" "12" "04D-D" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-D" BY REFERENCE ST-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-D" "X" "W-L" "49" "1" "05D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "06D-D" BY REFERENCE W-OK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-D" "N" "W-L" "51" "18" "06D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "07D-D" BY REFERENCE W-BKND1 "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "W-L" "13" "20" "D-D" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "X" "W-L" "27" "8" " " "D-TD"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "ZZZZ,ZZZ,ZZ9" "W-L" "36" "12" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE W-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "23" "0" "76" "04C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "19" "38" " " "05C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "19" "38" "D-NM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "42" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "42" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ACCEPT H-DATE FROM DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SHIT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           MOVE ZERO TO CHK.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 5
               CALL "PR_Open" RETURNING RESP
               GO TO M-500
           END-IF
           IF  W-ACT NOT = ZERO
               GO TO M-040
           END-IF.
       M-200.
           CALL "SD_Screen_Output" USING "SCTT11" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TTCD "A-TTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           MOVE W-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-200
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
           MOVE ZERO TO W-KIN.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-220.
      *           READ SHIT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               GO TO M-200
           END-IF
           IF  ST-TCD NOT = W-TCD
               GO TO M-220
           END-IF
           MOVE ST-FDD TO W-FRI W-FRID.
       M-240.
           MOVE " " TO W-KBN.
           IF  ST-TSC = 20
               MOVE "ｺ" TO W-KBN
           END-IF
           IF  ST-TSC = 21
               MOVE "ﾔ" TO W-KBN
           END-IF
           IF  ST-TSC = 22
               MOVE "ﾀ" TO W-KBN
           END-IF
           MOVE ST-MKD TO W-MAND.
           MOVE SPACE TO W-BKND1.
           IF  ST-SKC = 90
               MOVE "*" TO W-OK
               MOVE "＜　戻　り　＞　" TO W-BKN1
               GO TO M-280
           END-IF
           MOVE ST-BCD TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
           END-IF
           MOVE B-BNA TO W-BK.
           IF  ST-SKC = 00
               MOVE " " TO W-OK
               MOVE W-BK TO W-BKND1
               ADD ST-KIN TO W-KIN
           END-IF
           IF  ST-SKC = 50
               MOVE "*" TO W-OK
               MOVE W-BK TO W-BKND2
           END-IF
           IF  ST-SKC = 80
               MOVE "-" TO W-OK
               MOVE W-BK TO W-BKND2
           END-IF.
       M-280.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 22
               CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU
               GO TO M-320
           END-IF
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
       M-300.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-300
           END-IF
           CALL "SD_Screen_Output" USING "SCTT11" RETURNING RESU.
           CALL "SD_Output" USING "A-TTCD" A-TTCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU.
       M-320.
      *           READ SHIT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-340
           END-IF
           IF  W-TCD NOT = ST-TCD
               GO TO M-320
           END-IF
           IF  ST-FDD NOT = W-FRI
               MOVE ST-FDD TO W-FRI W-FRID
           ELSE
               MOVE SPACE TO W-FRIC
           END-IF
           GO TO M-240.
       M-340.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 22
               GO TO M-380
           END-IF
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
       M-360.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-360
           END-IF
           CALL "SD_Screen_Output" USING "SCTT11" RETURNING RESU.
           CALL "SD_Output" USING "A-TTCD" A-TTCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-380.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
           GO TO M-200.
       M-500.
           CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-PTCD "A-PTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "PR_Close" RETURNING RESP
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           MOVE W-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-500
           END-IF
           CALL "SD_Output" USING "D-PTNA" D-PTNA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
           MOVE ZERO TO W-KIN.
       M-520.
      *           READ SHIT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               GO TO M-500
           END-IF
           IF  W-TCD NOT = ST-TCD
               GO TO M-520
           END-IF
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           PERFORM S-20 THRU S-35.
       M-540.
           MOVE ST-FDD TO W-FRI.
           MOVE SPACE TO W-P.
           MOVE W-FRI TO P-FRI.
       M-560.
           MOVE ST-KEY TO P-KEY.
           IF  ST-TSC = 20
               MOVE "小　" TO P-KBN
           END-IF
           IF  ST-TSC = 21
               MOVE "約手" TO P-KBN
           END-IF
           IF  ST-TSC = 22
               MOVE "為手" TO P-KBN
           END-IF
           MOVE ST-MKD TO P-MAN.
           MOVE ST-KIN TO P-KIN.
           IF  ST-SKC = 90
               MOVE "*" TO P-OK
               MOVE "＜　戻　り　＞　" TO P-BKN2
               GO TO M-600
           END-IF
           MOVE ST-BCD TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
           END-IF
           MOVE B-BNA TO W-BK.
           IF  ST-SKC = 00
               MOVE W-BK TO P-BKN1
               ADD ST-KIN TO W-KIN
           END-IF
           IF  ST-SKC = 50
               MOVE "*" TO P-OK
               MOVE W-BK TO P-BKN2
           END-IF
           IF  ST-SKC = 80
               MOVE "-" TO P-OK
               MOVE W-BK TO P-BKN2
           END-IF.
       M-600.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO M-620
           END-IF
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-35.
           MOVE W-FRI TO P-FRI.
       M-620.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-640.
      *           READ SHIT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-660
           END-IF
           IF  W-TCD NOT = ST-TCD
               GO TO M-640
           END-IF
           IF  ST-FDD NOT = W-FRI
               GO TO M-540
           END-IF
           MOVE SPACE TO W-P.
           GO TO M-560.
       M-660.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
           MOVE SPACE TO W-P.
           MOVE "［　残　高　］　" TO P-TM.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO M-680
           END-IF
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-35.
       M-680.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-500.
       M-900.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 55
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-ADR.
           MOVE S-JSU TO W-JUP.
           MOVE ZERO TO CNT.
       S-25.
           ADD 1 TO CNT.
           IF  CNT NOT = 9
               IF  ADR1(CNT) NOT = SPACE
                   MOVE ADR1(CNT) TO ADR2(CNT)
                   GO TO S-25
               END-IF
           END-IF
           MOVE W-TCD TO H-TCD.
           MOVE W-ADR TO H-ADR.
           MOVE S-NAME TO H-TNA.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.

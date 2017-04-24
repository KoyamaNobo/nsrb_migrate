       IDENTIFICATION DIVISION.
       PROGRAM-ID. TST010.
      **********************************************
      *****     取引先別　受取手形　問合せ     *****
      *****         ( SCREEN : SCTT01 )        *****
      **********************************************
       AUTHOR. S-NAKAO.
       DATE-WRITTEN. 78-04-20.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　取引先別　受取手形　チェックリスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(012) VALUE SPACE.
       01  HEAD2.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  H-TCD          PIC  9(004).
           02  F              PIC  N(004) VALUE "　所在地".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-ADR          PIC  N(008).
           02  F              PIC  N(004) VALUE "取引先名".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-TNA          PIC  N(026).
           02  F              PIC  X(024) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(049) VALUE
                " 日　付  手形NO 種類   期　日       金　　額 落込".
           02  F              PIC  X(043) VALUE
                " 処理・銀行名        手　形　発　行　人　名".
           02  F              PIC  X(026) VALUE SPACE.
       01  W-P.
           02  P-UTD          PIC 99B99B99.
           02  F              PIC  X(002).
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TM           PIC  N(007).
           02  P-D     REDEFINES P-TM.
             03  P-KBN        PIC  N(002).
             03  F            PIC  X(002).
             03  P-MAN        PIC 99/99/99.
           02  P-KIN          PIC ZZ,ZZZ,ZZZ,ZZ9.
           02  F              PIC  X(003).
           02  P-OK           PIC  X(001).
           02  F              PIC  X(002).
           02  P-BKND         PIC  X(018).
           02  P-BKN   REDEFINES P-BKND  PIC  N(009).
           02  F              PIC  X(002).
           02  P-HAC          PIC  N(024).
       01  W-DATA.
           02  W-UTD          PIC  9(006).
           02  W-UTDD         PIC 99/99/99.
           02  W-UTDC  REDEFINES W-UTDD  PIC  X(008).
           02  W-MAN          PIC  9(006).
           02  W-MANC         PIC  X(008).
           02  W-MAND  REDEFINES W-MANC  PIC 99/99/99.
           02  W-MANT  REDEFINES W-MANC  PIC  N(004).
           02  W-KIN          PIC  9(010).
           02  W-TCD          PIC  9(004).
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-JUP.
             03  W-JUPD  OCCURS   8  PIC  N(001).
             03  F            PIC  N(016).
           02  W-ADR.
             03  W-ADRD  OCCURS   8  PIC  N(001).
           02  CNT            PIC  9(002).
           02  W-KBN          PIC  N(002).
           02  W-OK           PIC  X(001).
           02 W-BKN1.
             03  W-BK1        PIC  N(008).
             03  F            PIC  X(002).
           02  W-BKN2  REDEFINES W-BKN1.
             03  F            PIC  X(002).
             03  W-BK2        PIC  N(008).
           02  W-BKN3  REDEFINES W-BKN1.
             03  W-M          PIC  N(007).
             03  W-YBK        PIC  9(004).
           02  W-BNA          PIC  N(008).
           02  W-NA           PIC  N(026).
           02  W-HAC          PIC  X(005).
           02  W-L            PIC  9(002).
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LIBANK.
           COPY LSUKET.
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
                "＊＊＊　　　取引先別　受取手形帳　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "[   問合せ = 0  作表 = 5    ﾘﾀｰﾝ   ]".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-PTCD  PIC  9(004).
           02  A-TTCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-PM    PIC  X(037) VALUE
                "<   取引先ｺｰﾄﾞ        >      終了=PF9".
           02  D-PTNA  PIC  N(026).
           02  D-TTNA  PIC  N(026).
           02  FILLER.
             03  D-D.
               04  FILLER  PIC  X(008).
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(002).
               04  FILLER  PIC  X(008).
               04  FILLER  PIC ZZZZ,ZZZ,ZZ9 .
               04  FILLER  PIC  X(001).
               04  FILLER  PIC  X(018).
               04  FILLER  PIC  X(005).
             03  D-TD.
               04  FILLER  PIC  N(004).
               04  FILLER  PIC ZZZZ,ZZZ,ZZ9 .
           02  FILLER.
             03  D-NM    PIC  X(038) VALUE
                  "[  NEXT PAGE = ﾘﾀｰﾝ  END = BSKIP     ]".
             03  D-EM    PIC  X(038) VALUE
                  "                     [  END DATA     ]".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "344" " " " "  RETURNING RESU.
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
            "08C-MID" "X" "12" "14" "36" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "12" "41" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PTCD" "9" "15" "27" "4" "A-ACT" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PTCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TTCD" "9" "3" "13" "4" "A-PTCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TTCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "55" "1" "A-TTCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "297" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "15" "12" "37" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PTNA" "N" "17" "27" "52" "D-PM" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-PTNA" BY REFERENCE W-NA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TTNA" "N" "3" "18" "52" "D-PTNA" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TTNA" BY REFERENCE W-NA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L" "0" "80" "D-TTNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D" " " "W-L" "12" "60" " " "04C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-D" "X" "W-L" "6" "8" " " "D-D"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-D" BY REFERENCE W-UTDD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-D" "9" "W-L" "15" "4" "01D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-D" BY REFERENCE UT-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-D" "N" "W-L" "20" "4" "02D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "03D-D" BY REFERENCE W-KBN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-D" "X" "W-L" "25" "8" "03D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "04D-D" BY REFERENCE W-MAND "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-D" "ZZZZ,ZZZ,ZZ9" "W-L" "34" "12" "04D-D" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-D" BY REFERENCE UT-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-D" "X" "W-L" "47" "1" "05D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "06D-D" BY REFERENCE W-OK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-D" "X" "W-L" "49" "18" "06D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "07D-D" BY REFERENCE W-BKN1 "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-D" "X" "W-L" "68" "5" "07D-D" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "08D-D" BY REFERENCE W-HAC "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "W-L" "12" "20" "D-D" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "N" "W-L" "25" "8" " " "D-TD"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TD" BY REFERENCE W-MANT "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "ZZZZ,ZZZ,ZZ9" "W-L" "34" "12" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE W-KIN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "23" "0" "76" "04C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "21" "38" " " "05C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "21" "38" "D-NM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "92" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "92" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "15" "50" "E-ME99" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-000.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ACCEPT H-DATE FROM DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO UKET-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           MOVE ZERO TO CHK.
       M-020.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-020
           END-IF
           IF  W-ACT = 5
               CALL "PR_Open" RETURNING RESP
               GO TO M-500
           END-IF
           IF  W-ACT NOT = ZERO
               GO TO M-020
           END-IF.
       M-040.
           CALL "SD_Screen_Output" USING "SCTT01" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TTCD "A-TTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-040
           END-IF
           MOVE T-NAME TO W-NA.
       M-080.
           CALL "SD_Output" USING "D-TTNA" D-TTNA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" UKET-F_PNAME1 " " BY REFERENCE UKET-F_IDLST "0".
           MOVE ZERO TO W-KIN.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-100.
      *           READ UKET-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-F_IDLST UKET-F_PNAME1
               GO TO M-040
           END-IF
           IF  UT-TCD NOT = W-TCD
               GO TO M-100
           END-IF
           MOVE UT-UTD TO W-UTD W-UTDD.
       M-120.
           IF  UT-FDM = SPACE
               MOVE SPACE TO W-HAC
           ELSE
               MOVE "(ｳﾗﾃ)" TO W-HAC
           END-IF
           MOVE "　　" TO W-KBN.
           IF  UT-TSC = 10
               MOVE "小　" TO W-KBN
           END-IF
           IF  UT-TSC = 11
               MOVE "約手" TO W-KBN
           END-IF
           IF  UT-TSC = 12
               MOVE "為手" TO W-KBN
           END-IF
           IF  UT-TSC = 13
               MOVE "電債" TO W-KBN
           END-IF
           MOVE UT-MKD TO W-MAND.
           IF  UT-SKC = 19 OR 50 OR 60 OR 90
               MOVE "*" TO W-OK
           ELSE
               MOVE " " TO W-OK
               ADD UT-KIN TO W-KIN
           END-IF
           MOVE SPACE TO W-BKN1.
           IF  UT-SKC = ZERO
               GO TO M-180
           END-IF
           IF  UT-SKC = 19
               MOVE "　取立入金　　" TO W-M
               MOVE UT-SBC TO W-YBK
               GO TO M-180
           END-IF
           IF  UT-SKC = 20
               MOVE "担保差入　　　" TO W-M
               MOVE UT-SBC TO W-YBK
               GO TO M-180
           END-IF
           IF  UT-SKC = 90
               MOVE "　取り消し　　" TO W-M
           END-IF
           IF  UT-SKC = 60
               MOVE "＜不渡り＞　　" TO W-M
           END-IF
           IF  UT-SKC NOT = 60 AND 90
               GO TO M-140
           END-IF
           IF  UT-SBC NOT = ZERO
               MOVE UT-SBC TO W-YBK
           END-IF
           GO TO M-180.
       M-140.
           MOVE UT-SBC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
           END-IF
           MOVE B-BNA TO W-BNA.
       M-160.
           IF  UT-SKC = 32
               MOVE W-BNA TO W-BK1
           ELSE
               MOVE W-BNA TO W-BK2
           END-IF.
       M-180.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 21
               CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU
               GO TO M-220
           END-IF
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-F_IDLST UKET-F_PNAME1
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           CALL "SD_Screen_Output" USING "SCTT01" RETURNING RESU.
           CALL "SD_Output" USING "A-TTCD" A-TTCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TTNA" D-TTNA "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-D" D-D "p" RETURNING RESU.
       M-220.
      *           READ UKET-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-240
           END-IF
           IF  W-TCD NOT = UT-TCD
               GO TO M-220
           END-IF
           IF  UT-UTD NOT = W-UTD
               MOVE UT-UTD TO W-UTD W-UTDD
           ELSE
               MOVE SPACE TO W-UTDC
           END-IF
           GO TO M-120.
       M-240.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 21
               GO TO M-280
           END-IF
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-F_IDLST UKET-F_PNAME1
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           CALL "SD_Screen_Output" USING "SCTT01" RETURNING RESU.
           CALL "SD_Output" USING "A-TTCD" A-TTCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TTNA" D-TTNA "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-280.
           MOVE "残　　高" TO W-MANT.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-F_IDLST UKET-F_PNAME1.
           GO TO M-040.
       M-500.
           CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-PTCD "A-PTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "PR_Close" RETURNING RESP
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-500
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-500
           END-IF
           MOVE T-NAME TO W-NA.
           MOVE T-JSU TO W-JUP.
       M-540.
           CALL "SD_Output" USING "D-PTNA" D-PTNA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" UKET-F_PNAME1 " " BY REFERENCE UKET-F_IDLST "0".
           MOVE ZERO TO W-KIN.
       M-560.
      *           READ UKET-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-F_IDLST UKET-F_PNAME1
               GO TO M-500
           END-IF
           IF  W-TCD NOT = UT-TCD
               GO TO M-560
           END-IF
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           PERFORM S-20 THRU S-35.
       M-580.
           MOVE UT-UTD TO W-UTD.
           MOVE SPACE TO W-P.
           MOVE W-UTD TO P-UTD.
       M-600.
           MOVE UT-KEY TO P-KEY.
           IF  UT-TSC = 10
               MOVE "小　" TO P-KBN
           END-IF
           IF  UT-TSC = 11
               MOVE "約手" TO P-KBN
           END-IF
           IF  UT-TSC = 12
               MOVE "為手" TO P-KBN
           END-IF
           IF  UT-TSC = 13
               MOVE "電債" TO P-KBN
           END-IF
           MOVE UT-MKD TO P-MAN.
           MOVE UT-KIN TO P-KIN.
           IF  UT-SKC = 19 OR 50 OR 60 OR 90
               MOVE "*" TO P-OK
           ELSE
               MOVE " " TO P-OK
               ADD UT-KIN TO W-KIN
           END-IF
           MOVE SPACE TO W-BKN1.
           IF  UT-SKC = ZERO
               GO TO M-660
           END-IF
           IF  UT-SKC = 19
               MOVE "　取立入金　　" TO W-M
               MOVE UT-SBC TO W-YBK
               GO TO M-660
           END-IF
           IF  UT-SKC = 20
               MOVE "担保差入　　　" TO W-M
               MOVE UT-SBC TO W-YBK
               GO TO M-660
           END-IF
           IF  UT-SKC = 90
               MOVE "　取り消し　　" TO W-M
           END-IF
           IF  UT-SKC = 60
               MOVE "＜不渡り＞　　" TO W-M
           END-IF
           IF  UT-SKC NOT = 60 AND 90
               GO TO M-620
           END-IF
           IF  UT-SBC NOT = ZERO
               MOVE UT-SBC TO W-YBK
           END-IF
           GO TO M-660.
       M-620.
           MOVE UT-SBC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
           END-IF
           MOVE B-BNA TO W-BNA.
           IF  UT-SKC = 32
               MOVE W-BNA TO W-BK1
           ELSE
               MOVE W-BNA TO W-BK2
           END-IF.
       M-660.
           MOVE W-BKN1 TO P-BKN.
           MOVE UT-FDM TO P-HAC.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO M-680
           END-IF
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-35.
           MOVE W-UTD TO P-UTD.
       M-680.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-700.
      *           READ UKET-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" UKET-F_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-720
           END-IF
           IF  W-TCD NOT = UT-TCD
               GO TO M-700
           END-IF
           IF  W-UTD NOT = UT-UTD
               GO TO M-580
           END-IF
           MOVE SPACE TO W-P.
           GO TO M-600.
       M-720.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-F_IDLST UKET-F_PNAME1.
           MOVE SPACE TO W-P.
           MOVE "［　残高　］　" TO P-TM.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO M-740
           END-IF
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-35.
       M-740.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-500.
       M-900.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
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
           MOVE ZERO TO CNT.
       S-25.
           ADD 1 TO CNT.
           IF  CNT NOT = 9
               IF  W-JUPD(CNT) NOT = SPACE
                   MOVE W-JUPD(CNT) TO W-ADRD(CNT)
                   GO TO S-25
               END-IF
           END-IF
           MOVE W-TCD TO H-TCD.
           MOVE W-ADR TO H-ADR.
           MOVE W-NA TO H-TNA.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.

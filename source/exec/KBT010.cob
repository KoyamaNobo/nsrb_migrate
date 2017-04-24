       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBT010.
      *********************************************************
      *    PROGRAM         :  買掛金台帳　　　　　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBT01                          *
      *        変更　　　  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-NEN          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-GET          PIC Z9.
           02  F              PIC  N(021) VALUE
                "月分　買掛金台帳　（チェック用）　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(007) VALUE "仕　入　先　名".
           02  F              PIC  X(083) VALUE SPACE.
           02  F              PIC  X(013) VALUE "I----------  ".
           02  F              PIC  N(004) VALUE "買掛残高".
           02  F              PIC  X(013) VALUE "  ----------I".
       01  HEAD3.
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "材　　料　　名".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(003) VALUE "修正日".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(009) VALUE "単　価　　　金　額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "消費税　　　仕　入　　消費税　　　合　計".
       01  W-P1.
           02  P-15K1         PIC  X(005).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(095).
       01  W-P2.
           02  P-15K2         PIC  X(005).
           02  F              PIC  X(005).
           02  P-PEY          PIC Z9.
           02  F              PIC  X(001).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-TGP          PIC 99B99.
           02  P-SU           PIC ------,--9.99.
           02  P-T            PIC ----,--9.99.
           02  P-KIN          PIC ----,---,--9.
           02  P-SHZ          PIC -----,--9.
           02  P-ZKK          PIC ----,---,--9.
           02  P-SZZ          PIC --,---,--9.
           02  P-KZT          PIC ----,---,--9.
           02  P-20K          PIC  X(005).
       01  W-PT.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "　＊＊＊　　合　計　　＊＊＊".
           02  F              PIC  X(006) VALUE "   <  ".
           02  F              PIC  N(004) VALUE "仕入金額".
           02  P-SKT          PIC ----,---,--9.
           02  F              PIC  X(001).
           02  F              PIC  N(004) VALUE "消費税額".
           02  P-SZT          PIC --,---,--9.
           02  F              PIC  N(006) VALUE "　仕入支払額".
           02  P-HKT          PIC ----,---,--9.
           02  F              PIC  X(001).
           02  F              PIC  N(006) VALUE "消費税支払額".
           02  P-HZT          PIC --,---,--9.
           02  F              PIC  X(003) VALUE "  >".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-D.
           02  W-DATA.
             03  W-ZKK        PIC S9(009).
             03  W-SZZ        PIC S9(007).
             03  W-KZT        PIC S9(009).
             03  W-SKT        PIC S9(009).
             03  W-HKT        PIC S9(009).
             03  W-SZT        PIC S9(007).
             03  W-HZT        PIC S9(007).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GP.
               04  W-GET      PIC  9(002).
               04  W-PEY      PIC  9(002).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-PTC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NEC          PIC  9(001).
           02  W-KIN          PIC S9(009).
           02  W-SHZ          PIC S9(006).
           02  W-SCD          PIC  9(004).
           02  W-L            PIC  9(002).
           02  W-LL           PIC  9(002).
           02  W-HOSCD.
             03  W-HSCD       PIC  9(004).
             03  W-OSCD       PIC  9(004).
           02  W-JNA          PIC  N(024).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LISTM.
           COPY LIJM.
           COPY LSPF.
           COPY LSJSSW.
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
                "＊＊＊　　買　掛　金　台　帳　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(043) VALUE
                "<  作表 = 0  問合せ = 5  終り = 9   ﾘﾀｰﾝ  >".
       01  C-ACP.
           02  A-PTC   PIC  9(001).
           02  FILLER.
             03  A-HSCD  PIC  9(004).
             03  A-OSCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
           02  A-SCD   PIC  9(004).
           02  A-NEC   PIC  9(001).
       01  C-DSP.
           02  D-PM.
             03  FILLER  PIC  X(029) VALUE
                  "ｺｰﾄﾞ      より      迄打出し ".
             03  FILLER  PIC  X(021) VALUE
                  "確認 OK=1 NO=9   ﾘﾀｰﾝ".
           02  FILLER.
             03  D-SNA   PIC  N(024).
           02  FILLER.
             03  D-NG.
               04  FILLER  PIC 99 .
               04  FILLER  PIC Z9 .
           02  FILLER.
             03  D-P     PIC Z9 .
             03  D-JCD   PIC  9(006).
             03  D-JNA   PIC  N(024).
             03  D-TSD.
               04  FILLER  PIC ZZ .
               04  FILLER  PIC ZZ .
           02  FILLER.
             03  D-SU    PIC ZZZZZZ9.99- .
             03  D-T     PIC ZZZZZ9.99- .
             03  D-KIN   PIC ZZZZZZZZ9- .
             03  D-SHZ   PIC ZZZZZZ9- .
             03  D-ZKK   PIC ZZZZZZZZ9- .
             03  D-SZZ   PIC ZZZZZZ9- .
             03  D-KZT   PIC ZZZZZZZZ9- .
           02  D-TD.
             03  FILLER.
               04  FILLER  PIC ZZZ,ZZZ,ZZ9- .
               04  FILLER  PIC ZZZ,ZZZ,ZZ9- .
             03  FILLER.
               04  FILLER  PIC Z,ZZZ,ZZ9- .
               04  FILLER  PIC Z,ZZZ,ZZ9- .
           02  FILLER.
             03  D-NM    PIC  X(032) VALUE
                  "[  NEXT PAGE   ﾘﾀｰﾝ  ]   終了=Ⅱ".
             03  D-EM    PIC  X(032) VALUE
                  " [  END DATA   ﾘﾀｰﾝ  ]          ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  STM ﾅｼ  ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "309" " " " "  RETURNING RESU.
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
            "08C-MID" "X" "12" "7" "43" "07C-MID" " "  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "15" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PTC" "9" "12" "42" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PTC" BY REFERENCE W-PTC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "8" "A-PTC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HSCD" "9" "14" "20" "4" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HSCD" BY REFERENCE W-HSCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-OSCD" "9" "14" "30" "4" "A-HSCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-OSCD" BY REFERENCE W-OSCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "15" "34" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "2" "7" "4" "A-DMM" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEC" "9" "23" "44" "1" "A-SCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEC" BY REFERENCE W-NEC "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "337" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" " " "0" "0" "50" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PM" "X" "14" "15" "29" " " "D-PM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PM" "X" "15" "18" "21" "01D-PM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "2" "0" "48" "D-PM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "2" "21" "48" " " "02C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "1" "0" "4" "02C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "1" "0" "4" " " "03C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "99" "1" "60" "2" " " "D-NG"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "1" "64" "2" "01D-NG" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L" "0" "60" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-P" "Z9" "W-L" "2" "2" " " "04C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-P" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JCD" "9" "W-L" "5" "6" "D-P" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-JCD" BY REFERENCE JR-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JNA" "N" "W-L" "12" "48" "D-JCD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-JNA" BY REFERENCE W-JNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TSD" " " "W-L" "0" "4" "D-JNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TSD" "ZZ" "W-L" "61" "2" " " "D-TSD"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TSD" BY REFERENCE JR-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TSD" "ZZ" "W-L" "64" "2" "01D-TSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TSD" BY REFERENCE JR-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "W-LL" "0" "67" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZZZ9.99-" "W-LL" "7" "11" " " "05C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE JR-SU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T" "ZZZZZ9.99-" "W-LL" "19" "10" "D-SU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-T" BY REFERENCE JR-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZZ9-" "W-LL" "30" "10" "D-T" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHZ" "ZZZZZZ9-" "W-LL" "41" "8" "D-KIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHZ" BY REFERENCE W-SHZ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZKK" "ZZZZZZZZ9-" "W-LL" "50" "10" "D-SHZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZKK" BY REFERENCE W-ZKK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SZZ" "ZZZZZZ9-" "W-LL" "61" "8" "D-ZKK" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SZZ" BY REFERENCE W-SZZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KZT" "ZZZZZZZZ9-" "W-LL" "70" "10" "D-SZZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KZT" BY REFERENCE W-KZT "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "0" "0" "44" "05C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" " " "21" "0" "24" " " "D-TD"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-TD" "ZZZ,ZZZ,ZZ9-" "21" "44" "12" " " "01D-TD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-TD" BY REFERENCE W-SKT "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-TD" "ZZZ,ZZZ,ZZ9-" "21" "68" "12" "0101D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-TD" BY REFERENCE W-HKT "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" " " "22" "0" "20" "01D-TD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-TD" "Z,ZZZ,ZZ9-" "22" "46" "10" " " "02D-TD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-TD" BY REFERENCE W-SZT "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-TD" "Z,ZZZ,ZZ9-" "22" "70" "10" "0102D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-TD" BY REFERENCE W-HZT "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-DSP" " " "23" "0" "64" "D-TD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "30" "32" " " "07C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "30" "32" "D-NM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "183" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "183" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "16" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "75" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "75" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "SD_Accept" USING BY REFERENCE A-PTC "A-PTC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-05
           END-IF
           IF  W-PTC = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-PTC = 5
               GO TO MT-25
           END-IF
           IF  W-PTC NOT = ZERO
               GO TO M-05
           END-IF.
       M-10.
           MOVE ZERO TO W-HOSCD.
           CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-HSCD "A-HSCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-05
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-OSCD "A-OSCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-HSCD > W-OSCD
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
       MU-25.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" ST-M_PNAME1 "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       MU-30.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MU-95
           END-IF
           IF  W-HSCD > JR-SCD
               GO TO MU-30
           END-IF
           MOVE JR-NEN2 TO H-NEN.
           MOVE JR-GET TO H-GET.
           PERFORM S-10 THRU S-15.
       MU-35.
           MOVE ZERO TO W-DATA.
           MOVE JR-SCD TO W-SCD.
           MOVE JR-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊＊　仕入先マスター無し　＊＊＊" TO S-NAME
           END-IF
           MOVE JR-SCD TO ST-KEY.
      *           READ ST-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ST-M_PNAME1 BY REFERENCE ST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO ST-ZKZ ST-ZKZZ
           END-IF
           MOVE ST-ZKZ TO W-ZKK.
           MOVE ST-ZKZZ TO W-SZZ.
           COMPUTE W-KZT = W-ZKK + W-SZZ.
       MU-40.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K1.
           MOVE S-KEY TO P-SCD.
           MOVE S-NAME TO P-SNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  ZERO = W-ZKK AND W-SZZ
               GO TO MU-45
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　　　　＜　　前　月　繰　越　　＞" TO P-JNA.
           MOVE W-ZKK TO P-ZKK.
           MOVE W-SZZ TO P-SZZ.
           MOVE W-KZT TO P-KZT.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MU-45.
           PERFORM S-20 THRU S-40.
       MU-50.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO MU-95
           END-IF
           IF  W-SCD = JR-SCD
               GO TO MU-45
           END-IF
           MOVE W-SKT TO P-SKT.
           MOVE W-HKT TO P-HKT.
           MOVE W-SZT TO P-SZT.
           MOVE W-HZT TO P-HZT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO MU-55
           END-IF
           PERFORM S-05 THRU S-15.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       MU-55.
           MOVE SPACE TO SP-R.
           MOVE W-PT TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  W-OSCD < JR-SCD
               GO TO MU-95
           END-IF
           GO TO MU-35.
       MU-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           GO TO M-05.
       MT-25.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" ST-M_PNAME1 "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       MT-30.
           CALL "SD_Screen_Output" USING "SCBT01" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF ESTAT = BTB
               GO TO MT-95
           END-IF
           IF ESTAT = C2 OR PF9
               GO TO MT-95
           END-IF
           MOVE W-SCD TO S-KEY.
      *           START S-M KEY NOT < S-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            S-M_PNAME1 "S-KEY" " NOT < " S-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MT-30
           END-IF
      *           READ S-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO MT-30
           END-IF
           MOVE S-KEY TO W-SCD.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE S-KEY TO ST-KEY.
      *           READ ST-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ST-M_PNAME1 BY REFERENCE ST-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO ST-ZKZ ST-ZKZZ
           END-IF
           MOVE ST-ZKZ TO W-ZKK.
           MOVE ST-ZKZZ TO W-SZZ.
           COMPUTE W-KZT = W-ZKK + W-SZZ.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  ZERO = W-ZKK AND W-SZZ
               GO TO MT-35
           END-IF
           ADD 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           COMPUTE W-LL = W-L + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LL" "2" W-LL RETURNING RESU.
           MOVE  ALL "　"  TO W-JNA.
           MOVE "　　　　　＜　前月繰越　＞　" TO W-JNA.
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ZKK" D-ZKK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SZZ" D-SZZ "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KZT" D-KZT "p" RETURNING RESU.
       MT-35.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO MT-65
           END-IF
           IF  JR-SCD > W-SCD
               GO TO MT-65
           END-IF
           IF  JR-SCD NOT = W-SCD
               GO TO MT-35
           END-IF.
       MT-40.
           ADD 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           COMPUTE W-LL = W-L + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LL" "2" W-LL RETURNING RESU.
           IF  W-L > 20
               GO TO MT-60
           END-IF
           MOVE JR-DATE TO W-NGP.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           IF  JR-DC1 NOT = 3
               MOVE JR-KIN TO W-KIN
               MOVE JR-SHZ TO W-SHZ
               GO TO MT-45
           END-IF
           PERFORM S-50 THRU S-55.
           COMPUTE W-KIN = -1 * JR-KIN.
           COMPUTE W-SHZ = -1 * JR-SHZ.
           SUBTRACT JR-KIN FROM W-ZKK W-KZT.
           SUBTRACT JR-SHZ FROM W-SZZ W-KZT.
           ADD JR-KIN TO W-HKT.
           ADD JR-SHZ TO W-HZT.
           GO TO MT-50.
       MT-45.
           MOVE JR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ALL "　" TO J-NAME
               MOVE "　　＊＊　ＪＭ　なし　＊＊　" TO J-NAME
           END-IF
           MOVE J-NAME TO W-JNA.
           ADD JR-KIN TO W-ZKK W-SKT W-KZT.
           ADD JR-SHZ TO W-SZZ W-SZT W-KZT.
       MT-50.
           CALL "SD_Output" USING "D-P" D-P "p" RETURNING RESU.
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TSD" D-TSD "p" RETURNING RESU.
           IF  JR-DC1 NOT = 3
               CALL "SD_Output" USING "D-JCD" D-JCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHZ" D-SHZ "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ZKK" D-ZKK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SZZ" D-SZZ "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KZT" D-KZT "p" RETURNING RESU.
           GO TO MT-35.
       MT-60.
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NEC "A-NEC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO MT-70
           END-IF
           IF  ESTAT NOT = HTB
               GO TO MT-60
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT01" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO MT-40.
       MT-65.
           CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NEC "A-NEC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB
               GO TO MT-65
           END-IF.
       MT-70.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           GO TO MT-30.
       MT-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           GO TO M-05.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K.
           MOVE JR-PEY TO P-PEY.
           IF  JR-DC1 = 3
               COMPUTE W-KIN = -1 * JR-KIN
               COMPUTE W-SHZ = -1 * JR-SHZ
               GO TO S-30
           END-IF
           MOVE JR-JCD TO P-JCD.
           MOVE JR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　材料マスター無し　＊＊" TO P-JNA
               GO TO S-25
           END-IF
           MOVE J-NAME TO P-JNA.
       S-25.
           IF  JR-SGP NOT = ZERO
               MOVE JR-SGP TO P-TGP
           END-IF
           MOVE JR-SU TO P-SU.
           MOVE JR-T TO P-T.
           MOVE JR-KIN TO P-KIN.
           MOVE JR-SHZ TO P-SHZ.
           ADD JR-KIN TO W-ZKK W-SKT W-KZT.
           ADD JR-SHZ TO W-SZZ W-SZT W-KZT.
           GO TO S-35.
       S-30.
           PERFORM S-50 THRU S-55.
           MOVE W-JNA TO P-JNA.
           MOVE W-KIN TO P-KIN.
           MOVE W-SHZ TO P-SHZ.
           SUBTRACT JR-KIN FROM W-ZKK W-KZT.
           SUBTRACT JR-SHZ FROM W-SZZ W-KZT.
           ADD JR-KIN TO W-HKT.
           ADD JR-SHZ TO W-HZT.
       S-35.
           MOVE W-ZKK TO P-ZKK.
           MOVE W-SZZ TO P-SZZ.
           MOVE W-KZT TO P-KZT.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-40.
           EXIT.
       S-50.
           MOVE SPACE TO W-JNA.
           IF  JR-SC = 1
               MOVE "　　　　　［　現金支払　］　　" TO W-JNA
           END-IF
           IF  JR-SC = 2
               MOVE "　　　　　［　振込支払　］　　" TO W-JNA
           END-IF
           IF  JR-SC = 3
               MOVE "　　　　　［　小切手支払　］　" TO W-JNA
           END-IF
           IF  JR-SC = 4
               MOVE "　　　　　［　手形支払　］　　" TO W-JNA
           END-IF
           IF  JR-SC = 5
               MOVE "　　　　　［　売掛相殺　］　　" TO W-JNA
           END-IF
           IF  JR-SC = 6
               MOVE "　　　　　［　その他相殺　］　" TO W-JNA
           END-IF.
       S-55.
           EXIT.

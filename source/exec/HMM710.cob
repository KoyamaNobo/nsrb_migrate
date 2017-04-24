       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM710.
      *********************************************************
      *    PROGRAM         :  履物振替単価修正入力            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHM71                          *
      *    DATA WRITTN     :  00/05/31                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-DN               PIC  N(004) VALUE SPACE.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "【　　履物　振替単価　修正リスト　　】".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(005) VALUE "   P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　修正後".
           02  F              PIC  N(004) VALUE "振替単価".
           02  F              PIC  X(003) VALUE "  :".
           02  F              PIC  N(004) VALUE "　修正前".
           02  F              PIC  N(004) VALUE "振替単価".
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(003).
           02  P-NFT          PIC ZZZZ,ZZ9.
           02  F              PIC  X(002).
           02  P-X            PIC  X(001).
           02  F              PIC  X(001).
           02  F              PIC  X(003).
           02  P-OFT          PIC ZZZZ,ZZ9.
       01  W-DATA.
           02  W-POC          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-CC           PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-D.
             03  W-HCD        PIC  9(006).
             03  W-HCDD  REDEFINES W-HCD.
               04  W-HCD1     PIC  9(004).
               04  W-HCD2     PIC  9(002).
             03  W-FT         PIC  9(005).
             03  W-BC.
               04  W-BC1      PIC  9(002).
               04  W-BC2.
                 05  W-BC21   PIC  9(001).
                 05  W-BC22   PIC  9(001).
               04  W-BC3      PIC  9(002).
           02  W-FTO          PIC  9(005).
           02  W-FTD          PIC  9(005).
           02  W-HUH          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIHIM.
           COPY LIHUHM.
           COPY LSPF.
      *FD  HFTSF
       01  HFTSF_HMM710.
           02  HFTSF_PNAME1   PIC  X(005) VALUE "HFTSF".
           02  F              PIC  X(001).
           02  HFTSF_LNAME    PIC  X(012) VALUE "HFTSF_HMM710".
           02  F              PIC  X(001).
           02  HFTSF_KEY1     PIC  X(100) VALUE SPACE.
           02  HFTSF_KEY2     PIC  X(100) VALUE SPACE.
           02  HFTSF_SORT     PIC  X(100) VALUE SPACE.
           02  HFTSF_IDLST    PIC  X(100) VALUE SPACE.
           02  HFTSF_RES      USAGE  POINTER.
       01  HFTS-R.
           02  HFTS-KEY.
             03  HFTS-NC      PIC  9(001).
             03  HFTS-HCD     PIC  9(006).
             03  HFTS-HCDD  REDEFINES HFTS-HCD.
               04  HFTS-HCD1  PIC  9(004).
               04  HFTS-HCD2  PIC  9(002).
           02  HFTS-OLD.
             03  HFTS-FTO     PIC  9(005).
             03  F            PIC  X(019).
           02  HFTS-NEW.
             03  HFTS-FT      PIC  9(005).
             03  F            PIC  X(019).
           02 HFTS-BC.
             03  HFTS-BC1     PIC  9(002).
             03  HFTS-BC2.
               04  HFTS-BC21  PIC  9(001).
               04  HFTS-BC22  PIC  9(001).
             03  HFTS-BC3     PIC  9(002).
           02  F              PIC  X(003).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-HCD   PIC  9(006).
      *
           02  A-HCD1  PIC  9(004).
      *
           02  FILLER.
             03  A-FT    PIC  9(005).
      *
           02  A-NC    PIC  9(001).
           02  A-CC    PIC  9(001).
      *
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DN    PIC  N(004).
           02  D-NAME  PIC  N(024).
           02  FILLER.
             03  D-OLD.
               04  FILLER  PIC  Z(005).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(004).
             03  D-OCL.
               04  D-SNGC  PIC  X(004) VALUE "    ".
           02  FILLER.
             03  D-NEW.
               04  D-FT    PIC  Z(005).
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物　振替単価　修正入力　　＊＊＊".
           02  FILLER  PIC  X(030) VALUE
                "評価替え=1 , 単発修正=2   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  D-MID1.
             03  FILLER    PIC  N(003) VALUE "前回の".
             03  02D-MID1  PIC  N(004).
             03  FILLER    PIC  N(011) VALUE
                  "データ　クリア　　する".
             03  FILLER    PIC  X(013) VALUE "=1 , しない=5".
             03  FILLER    PIC  X(004) VALUE "ﾘﾀｰﾝ".
           02  D-MID1C.
             03  FILLER    PIC  X(036) VALUE
                  "                                    ".
             03  FILLER    PIC  X(020) VALUE
                  "                    ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(014) VALUE
                  "前回データエラー　更新済み？".
             03  E-ME2   PIC  X(026) VALUE
                  "***  ｻﾞｲｺ ｱﾘ ｼｭｳｾｲ ﾌｶ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME4   PIC  X(015) VALUE
                  "***  ﾐﾄｳﾛｸ  ***".
             03  E-ME5   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME6   PIC  X(021) VALUE
                  "***  ﾀﾝｶ ｶﾞ ﾁｶﾞｳ  ***".
             03  E-ME7   PIC  X(021) VALUE
                  "***  ﾋﾝﾒｲﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME9   PIC  X(022) VALUE
                  "***  ﾌﾘｶｴﾀﾝｶ ZERO  ***".
             03  E-ME10  PIC  X(020) VALUE
                  "***  新旧  同じ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(020) VALUE
                  "***  DELETE ｴﾗｰ  ***".
             03  E-KEY   PIC  9(006).
           COPY LSSEM.
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
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "19" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-ACT" "9" "3" "55" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCD" "9" "5" "12" "6" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCD1" "9" "5" "12" "4" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCD1" BY REFERENCE W-HCD1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-ACP" " " "13" "0" "5" "A-HCD1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-FT" "9" "13" "35" "5" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-FT" BY REFERENCE W-FT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-NC" "9" "8" "44" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-NC" BY REFERENCE W-NC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-CC" "9" "12" "59" "1" "A-NC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-CC" BY REFERENCE W-CC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "53" "1" "A-CC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "80" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-DN" "RN" "2" "66" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-DN" BY REFERENCE W-DN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-NAME" "N" "5" "25" "48" "D-DN" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-NAME" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03C-DSP" " " "9" "0" "19" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-OLD" " " "9" "0" "15" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-OLD" "Z" "9" "35" "5" " " "D-OLD" RETURNING RESU.
       CALL "SD_From" USING
            "01D-OLD" BY REFERENCE W-FTO "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-OLD" "9" "9" "49" "2" "01D-OLD" " " RETURNING RESU.
       CALL "SD_From" USING
            "02D-OLD" BY REFERENCE HI-BC1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-OLD" "9" "9" "52" "2" "02D-OLD" " " RETURNING RESU.
       CALL "SD_From" USING
            "03D-OLD" BY REFERENCE HI-BC2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-OLD" "9" "9" "55" "2" "03D-OLD" " " RETURNING RESU.
       CALL "SD_From" USING
            "04D-OLD" BY REFERENCE HI-BC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "05D-OLD" "9" "9" "58" "4" "04D-OLD" " " RETURNING RESU.
       CALL "SD_From" USING
            "05D-OLD" BY REFERENCE HI-SNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-OCL" " " "9" "0" "4" "D-OLD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SNGC" "X" "9" "58" "4" " " "D-OCL" RETURNING RESU.
       CALL "SD_Init" USING
            "04C-DSP" " " "13" "0" "5" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NEW" " " "13" "0" "5" " " "04C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-FT" "Z" "13" "35" "5" " " "D-NEW" RETURNING RESU.
       CALL "SD_From" USING
            "D-FT" BY REFERENCE W-FT "5" "0" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "12" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "X" "8" "19" "30" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "X" "20" "36" "22" "02C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "12" "0" "108" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID1" " " "12" "0" "53" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MID1" "N" "12" "8" "6" " " "D-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MID1" "N" "12" "14" "8" "01D-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-MID1" BY REFERENCE W-DN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-MID1" "N" "12" "22" "22" "02D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04D-MID1" "X" "12" "44" "13" "03D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05D-MID1" "X" "12" "60" "4" "04D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID1C" " " "12" "0" "56" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MID1C" "X" "12" "8" "36" " " "D-MID1C" RETURNING RESU.
       CALL "SD_Init" USING
         "02D-MID1C" "X" "12" "44" "20" "01D-MID1C" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "275" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "275" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "N" "24" "15" "28" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "15" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "21" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME6" "X" "24" "15" "21" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME7" "X" "24" "15" "21" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME8" "X" "24" "15" "17" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME9" "X" "24" "15" "22" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME10" "X" "24" "15" "20" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME11" "X" "24" "15" "19" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME12" "X" "24" "15" "21" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME13" "X" "24" "15" "20" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "45" "6" "E-ME13" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-NC "A-NC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-NC NOT = 1 AND 2
               GO TO M-040
           END-IF
           IF  W-NC = 1
               MOVE "評価替え" TO W-DN
           END-IF
           IF  W-NC = 2
               MOVE "単発修正" TO W-DN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           MOVE SPACE TO HFTS-KEY.
           MOVE W-NC TO HFTS-NC.
      *           START HFTSF KEY NOT < HFTS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HFTSF_PNAME1 "HFTS-KEY" " NOT < " HFTS-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "SD_Output" USING
                "D-MID1C" D-MID1C "p" RETURNING RESU
               GO TO M-080
           END-IF
      *           READ HFTSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "SD_Output" USING
                "D-MID1C" D-MID1C "p" RETURNING RESU
               GO TO M-080
           END-IF
           IF  HFTS-NC NOT = W-NC
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "SD_Output" USING
                "D-MID1C" D-MID1C "p" RETURNING RESU
               GO TO M-080
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           MOVE 1 TO W-DC.
           CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-CC "A-CC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-CC NOT = 1 AND 5
               GO TO M-080
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-DC = 1
                   GO TO M-060
               ELSE
                   GO TO M-040
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           IF  W-DMM = 9
               GO TO M-040
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-080
           END-IF
      *
           IF  W-DC = 0
               GO TO M-200
           END-IF
           IF  W-CC NOT = 1
               GO TO M-095
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           MOVE SPACE TO HFTS-KEY.
           MOVE W-NC TO HFTS-NC.
      *           START HFTSF KEY NOT < HFTS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HFTSF_PNAME1 "HFTS-KEY" " NOT < " HFTS-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               GO TO M-200
           END-IF.
       M-090.
      *           READ HFTSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               GO TO M-200
           END-IF
           IF  HFTS-NC NOT = W-NC
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               GO TO M-200
           END-IF
      *           DELETE HFTSF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HFTSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-090.
       M-095.
           CALL "DB_F_Open" USING
            "INPUT" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-100.
      *           READ HFTSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               GO TO M-200
           END-IF
           IF  HFTS-NC NOT = W-NC
               GO TO M-100
           END-IF
           MOVE HFTS-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  HI-FT = HFTS-FTO
               GO TO M-100
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO M-980.
       M-200.
           CALL "DB_F_Open" USING
            "I-O" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF W-NC = 2
               CALL "DB_F_Open" USING
                "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST
                "1" "HUH-KEY" BY REFERENCE HUH-KEY
           END-IF.
       M-220.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHM71" RETURNING RESU.
           CALL "SD_Output" USING "D-DN" D-DN "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-ACT = 9
               GO TO M-900
           END-IF
           IF  W-ACT = 4
               GO TO M-700
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3 AND 6
               GO TO M-220
           END-IF.
       M-240.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHM71" RETURNING RESU.
           CALL "SD_Output" USING "D-DN" D-DN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE ZERO TO W-D.
       M-250.
           IF  W-ACT = 6
               GO TO M-300
           END-IF.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-260
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE HI-BC TO W-BC.
      *
           MOVE W-NC TO HFTS-NC.
           MOVE W-HCD TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-280
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-260
           END-IF
           MOVE ZERO TO W-FTD W-FT.
           MOVE HFTS-FTO TO W-FTO.
           MOVE HFTS-FT TO W-FT W-FTD.
           CALL "SD_Output" USING "D-OLD" D-OLD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NEW" D-NEW "p" RETURNING RESU.
           IF  HI-SNG = ZERO
               CALL "SD_Output" USING
                "D-SNGC" D-SNGC "p" RETURNING RESU
           END-IF
           IF  W-ACT = 3
               GO TO M-520
           END-IF
           IF  W-NC = 2
               MOVE W-HCD TO HUH-KEY
               PERFORM S-20 THRU S-25
           END-IF
           IF  W-HUH = 9
               GO TO M-260
           END-IF
           GO TO M-380.
       M-280.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-260
           END-IF
           IF  HI-FT = ZERO
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-260
           END-IF
      *
           IF  W-NC = 2
               MOVE W-HCD TO HUH-KEY
               PERFORM S-20 THRU S-25
           END-IF
           IF  W-HUH = 9
               GO TO M-260
           END-IF
      *
           PERFORM S-30 THRU S-35.
           GO TO M-380.
       M-300.
           CALL "SD_Accept" USING BY REFERENCE A-HCD1 "A-HCD1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF ESTAT = BTB
               GO TO M-220
           END-IF
           IF ESTAT NOT = HTB AND SKP
               GO TO M-300
           END-IF
      *
           MOVE ZERO TO W-HCD2.
           MOVE W-HCD TO HI-KEY.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           IF  HI-HCD1 NOT = W-HCD1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
      *
           MOVE HI-HCD TO W-HCD.
           MOVE HI-BC TO W-BC.
           PERFORM S-30 THRU S-35.
      *
           MOVE W-NC TO HFTS-NC.
           MOVE HI-KEY TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-320
           END-IF
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-300.
       M-320.
           IF  HI-FT = ZERO
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           IF  W-NC = 2
               MOVE HI-HCD TO HUH-KEY
               PERFORM S-20 THRU S-25
           END-IF
           IF  W-HUH = 9
               GO TO M-300
           END-IF.
       M-340.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-380
           END-IF
           IF  HI-HCD1 NOT = W-HCD1
               GO TO M-380
           END-IF
           MOVE W-NC TO HFTS-NC.
           MOVE HI-KEY TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-360
           END-IF
           CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-300.
       M-360.
           IF  HI-FT = ZERO
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           IF  W-NC = 2
               MOVE HI-HCD TO HUH-KEY
               PERFORM S-20 THRU S-25
           END-IF
           IF  W-HUH = 9
               GO TO M-300
           END-IF
           IF  HI-FT = W-FTO
               GO TO M-340
           END-IF
           CALL "SD_Output" USING "E-ME6" E-ME6 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-300.
       M-380.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-FT "A-FT" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-250
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           CALL "SD_Output" USING "D-FT" D-FT "p" RETURNING RESU.
           IF  W-FTO = W-FT
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-500
           END-IF.
       M-520.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-260
               ELSE
                   GO TO M-500
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-520
           END-IF
           IF  W-DMM = 9
               GO TO M-250
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-520
           END-IF.
       M-540.
           IF  W-ACT NOT = 1
               GO TO M-580
           END-IF
           MOVE ZERO TO HFTS-R.
           MOVE W-HCD TO HFTS-HCD.
           MOVE W-FTO TO HFTS-FTO
           MOVE W-FT TO HFTS-FT.
           MOVE W-BC TO HFTS-BC.
           MOVE W-NC TO HFTS-NC.
      *           WRITE HFTS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HFTSF_PNAME1 HFTSF_LNAME HFTS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-560
           END-IF
           GO TO M-240.
       M-560.
           IF  ERR-STAT NOT = "24"
               GO TO M-900
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           MOVE "HFTSF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           GO TO M-540.
       M-580.
           IF W-ACT NOT = 2
               GO TO M-600
           END-IF
           MOVE W-FT TO HFTS-FT.
           MOVE W-NC TO HFTS-NC.
      *           REWRITE HFTS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HFTSF_PNAME1 HFTSF_LNAME HFTS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-240.
       M-600.
           IF  W-ACT NOT = 3
               GO TO M-620
           END-IF
      *           DELETE HFTSF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HFTSF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-240.
       M-620.
           IF  W-ACT NOT = 6
               GO TO M-240
           END-IF
           MOVE ZERO TO W-HCD2.
           MOVE W-HCD TO HI-KEY.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF.
       M-640.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-240
           END-IF
           IF  HI-HCD1 NOT = W-HCD1
               GO TO M-240
           END-IF
      *
           MOVE ZERO TO HFTS-R.
           MOVE HI-HCD TO HFTS-HCD.
           MOVE W-FTO TO HFTS-FTO.
           MOVE W-FT TO HFTS-FT.
           MOVE HI-BC TO HFTS-BC.
           MOVE W-NC TO HFTS-NC.
      *           WRITE HFTS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HFTSF_PNAME1 HFTSF_LNAME HFTS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-660
           END-IF
           GO TO M-640.
       M-660.
           IF  ERR-STAT NOT = "24"
               GO TO M-900
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           MOVE "HFTSF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           GO TO M-640.
       M-700.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
       M-720.
      *           READ HFTSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-740
           END-IF
           IF  W-NC NOT = HFTS-NC
               GO TO M-720
           END-IF
           IF  W-POC = 0
               MOVE 9 TO W-POC
               ACCEPT H-DATE FROM DATE
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           END-IF
           MOVE HFTS-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           MOVE SPACE TO W-P.
           MOVE HFTS-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE ":" TO P-X.
           MOVE HFTS-FTO TO P-OFT.
           MOVE HFTS-FT TO P-NFT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-720.
       M-740.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           GO TO M-220.
       M-900.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  W-NC = 2
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
           END-IF
           IF  W-POC = 5
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO W-HUH.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE ZERO TO HUH-ZS
           END-IF
           IF  HUH-ZS NOT = ZERO
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-HUH
           END-IF.
       S-25.
           EXIT.
       S-30.
           MOVE ZERO TO W-FTO.
           MOVE HI-FT TO W-FTO.
           CALL "SD_Output" USING "D-OLD" D-OLD "p" RETURNING RESU.
           IF  HI-SNG = ZERO
               CALL "SD_Output" USING "D-SNGC" D-SNGC "p" RETURNING RESU
           END-IF.
       S-35.
           EXIT.

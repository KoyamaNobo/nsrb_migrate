       IDENTIFICATION DIVISION.
       PROGRAM-ID. JT205U.
      ******************************************************
      *****     ＥＯＳ累積ファイル他　削除             *****
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       01  W-DATA.
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-DATE         PIC  9(008).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LSKHAT.
      *FD  WMJCRF
       01  WMJCRF_JT205U.
           02  WMJCRF_PNAME1         PIC  X(006) VALUE "WMJCRF".
           02  F                     PIC  X(001).
           02  WMJCRF_LNAME          PIC  X(013) VALUE "WMJCRF_JT205U".
           02  F                     PIC  X(001).
           02  WMJCRF_KEY1           PIC  X(100) VALUE SPACE.
           02  WMJCRF_SORT           PIC  X(100) VALUE SPACE.
           02  WMJCRF_IDLST          PIC  X(100) VALUE SPACE.
           02  WMJCRF_RES            USAGE  POINTER.
       01  WMJCR-R.
           02  F                     PIC X(1301).
           02  WMJCR-DATE            PIC 9(06).
       77  F                         PIC X(01).
      *FD  JCAWRF
       01  JCAWRF_JT205U.
           02  JCAWRF_PNAME1         PIC  X(006) VALUE "JCAWRF".
           02  F                     PIC  X(001).
           02  JCAWRF_LNAME          PIC  X(013) VALUE "JCAWRF_JT205U".
           02  F                     PIC  X(001).
           02  JCAWRF_KEY1           PIC  X(100) VALUE SPACE.
           02  JCAWRF_SORT           PIC  X(100) VALUE SPACE.
           02  JCAWRF_IDLST          PIC  X(100) VALUE SPACE.
           02  JCAWRF_RES            USAGE  POINTER.
       01  JCAWR-R.
           02  F                     PIC X(335).
           02  JCAWR-DATE            PIC 9(06).
       77  F                         PIC X(01).
      *FD  JCANRF
       01  JCANRF_JT205U.
           02  JCANRF_PNAME1         PIC  X(006) VALUE "JCANRF".
           02  F                     PIC  X(001).
           02  JCANRF_LNAME          PIC  X(013) VALUE "JCANRF_JT205U".
           02  F                     PIC  X(001).
           02  JCANRF_KEY1           PIC  X(100) VALUE SPACE.
           02  JCANRF_SORT           PIC  X(100) VALUE SPACE.
           02  JCANRF_IDLST          PIC  X(100) VALUE SPACE.
           02  JCANRF_RES            USAGE  POINTER.
       01  JCANR-R.
           02  F                     PIC X(335).
           02  JCANR-DATE            PIC 9(06).
       77  F                         PIC X(01).
      *FD  JCAARF
       01  JCAARF_JT205U.
           02  JCAARF_PNAME1         PIC  X(006) VALUE "JCAARF".
           02  F                     PIC  X(001).
           02  JCAARF_LNAME          PIC  X(013) VALUE "JCAARF_JT205U".
           02  F                     PIC  X(001).
           02  JCAARF_KEY1           PIC  X(100) VALUE SPACE.
           02  JCAARF_SORT           PIC  X(100) VALUE SPACE.
           02  JCAARF_IDLST          PIC  X(100) VALUE SPACE.
           02  JCAARF_RES            USAGE  POINTER.
       01  JCAAR-R.
           02  F                     PIC X(128).
           02  JCAAR-DATE            PIC 9(06).
           02  F                     PIC X(036).
       77  F                         PIC X(01).
      *****   ナフコ数量累積ファイル 53/4   ****************************
      *FD  NSURYORF
       01  NSURYORF_JT205U.
           02  NSURYORF_PNAME1  PIC  X(008) VALUE "NSURYORF".
           02  F                PIC  X(001).
           02  NSURYORF_LNAME   PIC  X(015) VALUE "NSURYORF_JT205U".
           02  F                PIC  X(001).
           02  NSURYORF_KEY1    PIC  X(100) VALUE SPACE.
           02  NSURYORF_SORT    PIC  X(100) VALUE SPACE.
           02  NSURYORF_IDLST   PIC  X(100) VALUE SPACE.
           02  NSURYORF_RES     USAGE  POINTER.
       01  NSURYOR-R.
           02  NSURYOR-01       PIC 9(06).
           02  NSURYOR-02       PIC 9(06).
           02  NSURYOR-03       PIC 9(03).
           02  NSURYOR-04       PIC 9(08).
           02  NSURYOR-05       PIC 9(06).
           02  NSURYOR-06       PIC 9(08).
           02  NSURYOR-07       PIC 9(04).
           02  NSURYOR-08       PIC 9(02).
           02  NSURYOR-09       PIC 9(02).
           02  F                PIC 9(02).
           02  NSURYOR-10       PIC 9(06).
       77  F                    PIC X(01).
      *****   ナフコ箱数累積ファイル 44/5   ****************************
      *FD  NHAKORF
       01  NHAKORF_JT205U.
           02  NHAKORF_PNAME1   PIC  X(007) VALUE "NHAKORF".
           02  F                PIC  X(001).
           02  NHAKORF_LNAME    PIC  X(014) VALUE "NHAKORF_JT205U".
           02  F                PIC  X(001).
           02  NHAKORF_KEY1     PIC  X(100) VALUE SPACE.
           02  NHAKORF_SORT     PIC  X(100) VALUE SPACE.
           02  NHAKORF_IDLST    PIC  X(100) VALUE SPACE.
           02  NHAKORF_RES      USAGE  POINTER.
       01  NHAKOR-R.
           02  NHAKOR-01        PIC 9(06).
           02  NHAKOR-02        PIC 9(06).
           02  NHAKOR-03        PIC 9(03).
           02  NHAKOR-04        PIC 9(01).
           02  NHAKOR-05        PIC 9(06).
           02  NHAKOR-06        PIC 9(08).
           02  NHAKOR-07        PIC 9(04).
           02  NHAKOR-08        PIC 9(02).
           02  F                PIC 9(02).
           02  NHAKOR-10        PIC 9(06).
       77  F                    PIC X(01).
      *****   ナフコ数量訂正累積ファイル 128/2  ************************
      *FD  TEISEIRF
       01  TEISEIRF_JT205U.
           02  TEISEIRF_PNAME1  PIC  X(009) VALUE "NTEISEIRF".
           02  F                PIC  X(001).
           02  TEISEIRF_LNAME   PIC  X(015) VALUE "TEISEIRF_JT205U".
           02  F                PIC  X(001).
           02  TEISEIRF_KEY1    PIC  X(100) VALUE SPACE.
           02  TEISEIRF_SORT    PIC  X(100) VALUE SPACE.
           02  TEISEIRF_IDLST   PIC  X(100) VALUE SPACE.
           02  TEISEIRF_RES     USAGE  POINTER.
       01  TEISEIR-R.
           02  TEISEIR-01       PIC 9(06).
           02  TEISEIR-02       PIC 9(06).
           02  TEISEIR-03       PIC 9(03).
           02  TEISEIR-04       PIC 9(08).
           02  TEISEIR-05       PIC 9(05).
           02  TEISEIR-051      PIC 9(01).
           02  TEISEIR-06       PIC 9(08).
           02  TEISEIR-07       PIC 9(04).
           02  TEISEIR-08       PIC 9(02).
           02  TEISEIR-09       PIC 9(02).
           02  TEISEIR-21       PIC X(25).
           02  TEISEIR-22       PIC X(24).
           02  TEISEIR-23       PIC X(08).
           02  F                PIC X(18).
           02  F                PIC 9(02).
           02  TEISEIR-10       PIC 9(06).
       77  F                    PIC X(01).
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
           02  FILLER  PIC  N(015) VALUE
                "　ＥＯＳ累積ファイル他　削除　".
       01  C-DSP.
           02  D-NGP.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  N(001) VALUE "年".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  N(001) VALUE "月".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  N(002) VALUE "日迄".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME10  PIC  X(028) VALUE
                  "***  JCAWRF REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(028) VALUE
                  "***  JCANRF REWRITE ｴﾗｰ  ***".
             03  E-ME14  PIC  X(028) VALUE
                  "***  JCAARF REWRITE ｴﾗｰ  ***".
             03  E-ME15  PIC  X(027) VALUE
                  "***  SK-HAT DELETE ｴﾗｰ  ***".
             03  E-ME16  PIC  X(028) VALUE
                  "***  WMJCRF REWRITE ｴﾗｰ  ***".
             03  E-ME17  PIC  X(030) VALUE
                  "***  NSURYORF REWRITE ｴﾗｰ  ***".
             03  E-ME18  PIC  X(029) VALUE
                  "***  NHAKORF REWRITE ｴﾗｰ  ***".
             03  E-ME19  PIC  X(031) VALUE
                  "***  NTEISEIRF REWRITE ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RN" "1" "25" "30" " " "C-MID" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "14" "0" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGP" "9" "14" "29" "2" " " "D-NGP" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGP" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGP" "N" "14" "31" "2" "01D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGP" "9" "14" "34" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-NGP" "N" "14" "36" "2" "03D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-NGP" "9" "14" "39" "2" "04D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-NGP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-NGP" "N" "14" "41" "4" "05D-NGP" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "291" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "291" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "28" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "28" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "28" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "27" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "28" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" "X" "24" "15" "30" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" "X" "24" "15" "29" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME19" "X" "24" "15" "31" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME19" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-010.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           MOVE ZERO TO W-NGP.
           ACCEPT W-NGPS FROM DATE.
           COPY LIBCPR.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-DATE.
      *
           MOVE W-DATE TO W-NGP.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
      *
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" WMJCRF_PNAME1 " " BY REFERENCE WMJCRF_IDLST "0".
       M-530.
      *           READ WMJCRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WMJCRF_PNAME1 BY REFERENCE WMJCR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-540
           END-IF
           IF  WMJCR-DATE > W-NGPS
               GO TO M-540
           END-IF
      *
           MOVE X"FF" TO WMJCR-R.
      *           REWRITE WMJCR-R.
      *///////////////
           CALL "DB_Update" USING
            WMJCRF_PNAME1 WMJCRF_LNAME WMJCR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE WMJCRF_IDLST WMJCRF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-530.
       M-540.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCRF_IDLST WMJCRF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" JCAWRF_PNAME1 " " BY REFERENCE JCAWRF_IDLST "0".
       M-550.
      *           READ JCAWRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAWRF_PNAME1 BY REFERENCE JCAWR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-560
           END-IF
           IF  JCAWR-DATE > W-NGPS
               GO TO M-560
           END-IF
      *
           MOVE X"FF" TO JCAWR-R.
      *           REWRITE JCAWR-R.
      *///////////////
           CALL "DB_Update" USING
            JCAWRF_PNAME1 JCAWRF_LNAME JCAWR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAWRF_IDLST JCAWRF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-550.
       M-560.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAWRF_IDLST JCAWRF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" JCANRF_PNAME1 " " BY REFERENCE JCANRF_IDLST "0".
       M-610.
      *           READ JCANRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCANRF_PNAME1 BY REFERENCE JCANR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-620
           END-IF
           IF  JCANR-DATE > W-NGPS
               GO TO M-620
           END-IF
      *
           MOVE X"FF" TO JCANR-R.
      *           REWRITE JCANR-R.
      *///////////////
           CALL "DB_Update" USING
            JCANRF_PNAME1 JCANRF_LNAME JCANR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCANRF_IDLST JCANRF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-610.
       M-620.
           CALL "DB_F_Close" USING
            BY REFERENCE JCANRF_IDLST JCANRF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" JCAARF_PNAME1 " " BY REFERENCE JCAARF_IDLST "0".
       M-630.
      *           READ JCAARF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAARF_PNAME1 BY REFERENCE JCAAR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-640
           END-IF
           IF  JCAAR-DATE > W-NGPS
               GO TO M-640
           END-IF
      *
           MOVE X"FF" TO JCAAR-R.
      *           REWRITE JCAAR-R.
      *///////////////
           CALL "DB_Update" USING
            JCAARF_PNAME1 JCAARF_LNAME JCAAR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAARF_IDLST JCAARF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-630.
       M-640.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAARF_IDLST JCAARF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" SK-HAT_PNAME1 " " BY REFERENCE SK-HAT_IDLST "1"
            "HAT-KEY" BY REFERENCE HAT-KEY.
       M-670.
      *           READ SK-HAT NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SK-HAT_PNAME1 BY REFERENCE HAT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-680
           END-IF
           IF  HAT-25     > W-NGP
               GO TO M-670
           END-IF
      *
      *           DELETE SK-HAT INVALID KEY
      *///////////////
           CALL "DB_Delete" USING SK-HAT_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE SK-HAT_IDLST SK-HAT_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-670.
       M-680.
           CALL "DB_F_Close" USING
            BY REFERENCE SK-HAT_IDLST SK-HAT_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" NSURYORF_PNAME1 " " BY REFERENCE NSURYORF_IDLST "0".
       M-690.
      *           READ NSURYORF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NSURYORF_PNAME1 BY REFERENCE NSURYOR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-700
           END-IF
           IF  NSURYOR-10 > W-NGPS
               GO TO M-700
           END-IF
           MOVE X"FF" TO NSURYOR-R.
      *           REWRITE NSURYOR-R.
      *///////////////
           CALL "DB_Update" USING
            NSURYORF_PNAME1 NSURYORF_LNAME NSURYOR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE NSURYORF_IDLST NSURYORF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-690.
       M-700.
           CALL "DB_F_Close" USING
            BY REFERENCE NSURYORF_IDLST NSURYORF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" NHAKORF_PNAME1 " " BY REFERENCE NHAKORF_IDLST "0".
       M-710.
      *           READ NHAKORF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NHAKORF_PNAME1 BY REFERENCE NHAKOR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-720
           END-IF
           IF  NHAKOR-10 > W-NGPS
               GO TO M-720
           END-IF
           MOVE X"FF" TO NHAKOR-R.
      *           REWRITE NHAKOR-R.
      *///////////////
           CALL "DB_Update" USING
            NHAKORF_PNAME1 NHAKORF_LNAME NHAKOR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE NHAKORF_IDLST NHAKORF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-710.
       M-720.
           CALL "DB_F_Close" USING
            BY REFERENCE NHAKORF_IDLST NHAKORF_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" TEISEIRF_PNAME1 " " BY REFERENCE TEISEIRF_IDLST "0".
       M-730.
      *           READ TEISEIRF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TEISEIRF_PNAME1 BY REFERENCE TEISEIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-740
           END-IF
           IF  TEISEIR-10 > W-NGPS
               GO TO M-740
           END-IF
           MOVE X"FF" TO TEISEIR-R.
      *           REWRITE TEISEIR-R.
      *///////////////
           CALL "DB_Update" USING
            TEISEIRF_PNAME1 TEISEIRF_LNAME TEISEIR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE TEISEIRF_IDLST TEISEIRF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-950
           END-IF
           GO TO M-730.
       M-740.
           CALL "DB_F_Close" USING
            BY REFERENCE TEISEIRF_IDLST TEISEIRF_PNAME1.
       M-950.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

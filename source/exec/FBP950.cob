       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBP950.
      ****************************************************
      *****    銀行　入出金取引伝送データ　ＣＨＥＣＫ*****
      *****        (  中国銀行  JS-SIGN=0  )         *****
      *****        (  商工中金  JS-SIGN=1  )         *****
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN          PIC  9(001).
       77  JS-SU            PIC  9(001).
       77  ERR-STAT         PIC  X(002).
       77  W-EC             PIC  9(001) VALUE 0.
       01  W-R1.
           02  W1-DC        PIC  9(001).
           02  F            PIC  X(003).
           02  W1-NGP       PIC  9(006).
           02  W1-SNGP      PIC  9(006).
           02  W1-ENGP      PIC  9(006).
           02  W1-BKC       PIC  9(004).
           02  W1-BKN       PIC  X(015).
           02  W1-HSC       PIC  9(003).
           02  W1-HSN       PIC  X(015).
           02  F            PIC  X(003).
           02  W1-YKS       PIC  9(001).
           02  W1-KNO       PIC  9(010).
           02  W1-KNA       PIC  X(040).
           02  W1-PMC       PIC  9(001).
           02  F            PIC  X(001).
           02  W1-ZKIN      PIC  9(014).
           02  F            PIC  X(071).
       01  W-DATA.
           02  W-BANK       PIC  N(004).
           02  W-BANKD      PIC  N(004).
           02  W-SU         PIC  N(001).
           02  W-DMM        PIC  9(001).
           COPY LSTAT.
      *
      *FD  RNOUSTF
       01  RNOUSTF_FBP950.
           02  RNOUSTF_PNAME1 PIC  X(007) VALUE "RNOUSTF".
           02  F              PIC  X(001).
           02  RNOUSTF_LNAME  PIC  X(014) VALUE "RNOUSTF_FBP950".
           02  F              PIC  X(001).
           02  RNOUSTF_KEY1   PIC  X(100) VALUE SPACE.
           02  RNOUSTF_SORT   PIC  X(100) VALUE SPACE.
           02  RNOUSTF_IDLST  PIC  X(100) VALUE SPACE.
           02  RNOUSTF_RES    USAGE  POINTER.
       01  RNOUST-R.
           02  NS-DC          PIC  9(001).
           02  F              PIC  X(199).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　　入出金取引　受信　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　（　回目）　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-BANK  PIC  N(004).
           02  D-SU    PIC  N(001).
           02  D-ZDM.
             03  FILLER  PIC  N(004).
             03  FILLER  PIC  N(010) VALUE
                    "ＤＡＴＡ　有り　！！".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "344" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "22" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "38" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BANK" "N" "6" "20" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BANK" BY REFERENCE W-BANK "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "N" "7" "30" "2" "D-BANK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZDM" " " "15" "0" "28" "D-SU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZDM" "N" "15" "18" "8" " " "D-ZDM" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-ZDM" BY REFERENCE W-BANKD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZDM" "N" "15" "28" "20" "01D-ZDM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           ACCEPT JS-SU FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  JS-SU NOT = 1 AND 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE SPACE TO W-BANK.
           IF  JS-SIGN = 0
               MOVE   "中国銀行" TO W-BANK
           END-IF
           IF  JS-SIGN = 1
               MOVE   "商工中金" TO W-BANK
           END-IF
           IF  JS-SU = 1
               MOVE   "１" TO W-SU
           END-IF
           IF  JS-SU = 2
               MOVE   "２" TO W-SU
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BANK" D-BANK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" RNOUSTF_PNAME1 " " BY REFERENCE RNOUSTF_IDLST "0".
      *
      *           READ RNOUSTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUSTF_PNAME1 BY REFERENCE RNOUST-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1
               GO TO M-10
           END-IF
           MOVE SPACE TO W-BANKD.
           INITIALIZE W-R1.
           MOVE RNOUST-R TO W-R1.
           IF  W1-BKC = 0168
               MOVE   "中国銀行" TO W-BANKD
           END-IF
           IF  W1-BKC = 2004
               MOVE   "商工中金" TO W-BANKD
           END-IF
           CALL "SD_Output" USING "D-ZDM" D-ZDM "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1.
           GO TO M-95.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

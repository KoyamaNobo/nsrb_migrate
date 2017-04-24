       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG000.
      *********************************************************
      *    PROGRAM         :  月報表紙                        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=履物 , 1=工品他               *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  H-BS           PIC  N(004) VALUE SPACE.
           02  F              PIC  X(028) VALUE SPACE.
           02  H-MID          PIC  N(012).
           02  F              PIC  X(008) VALUE X"1A26212068212078".
       01  W-DATA.
           02  W-MID0.
             03  F            PIC  N(001) VALUE "’".
             03  W-PNEN       PIC  N(002).
             03  F            PIC  N(001) VALUE "年".
             03  W-PGET       PIC  N(002).
             03  F            PIC  N(006) VALUE "月度　実績表".
           02  W-NG.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGD.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  Z(002).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
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
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　月報　表題　作成　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(001) VALUE "'".
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  N(001) VALUE "年".
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  N(006) VALUE "月分　実績表".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
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
            "C-MID" " " "0" "0" "293" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "36" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "36" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "36" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "36" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "36" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "36" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" " " "14" "0" "19" "07C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0108C-MID" "X" "14" "18" "1" " " "08C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
           "0208C-MID" "9" "14" "19" "2" "0108C-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0208C-MID" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0308C-MID" "N" "14" "21" "2" "0208C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0408C-MID" "Z" "14" "24" "2" "0308C-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0408C-MID" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "0508C-MID" "N" "14" "26" "12" "0408C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "17" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "34" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           MOVE ZERO TO W-NG.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NG.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               MOVE 1 TO W-DMM
               CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU
               MOVE "【工品】" TO H-BS
               GO TO M-15
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
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
       M-15.
           MOVE W-NEN TO W-NEND.
           MOVE W-GET TO W-GETD.
           MOVE W-NEND TO W-PNEN.
           MOVE W-GETD TO W-PGET
           MOVE W-MID0 TO H-MID.
           CALL "PR_Open" RETURNING RESP.
      *
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "10" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.

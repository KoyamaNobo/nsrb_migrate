       IDENTIFICATION DIVISION.
       PROGRAM-ID. CFLC10.
      *************************************************************
      **   ä»à’ïœä∑èàóù                                          **
      **    #FLCNVãNìÆ (CBLRUN)                                  **
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  STN-NO.
           03  STN-NO-1       PIC  X(003) VALUE SPACE.
           03  STN-NO-2       PIC  9(003) VALUE ZERO.
       01  W-DATA.
           02  HIZUKE.
             03  YY           PIC  9(002) VALUE ZERO.
             03  MM           PIC  9(002) VALUE ZERO.
             03  DD           PIC  9(002) VALUE ZERO.
           02  K-MSG          PIC  X(040) VALUE SPACE.
           02  W-KBN          PIC  9(002) VALUE ZERO.
           02  W-FID1.
             03  W-FID11      PIC  X(006) VALUE SPACE.
             03  W-FID12      PIC  9(003).
             03  W-FID13      PIC  X(003) VALUE SPACE.
           02  W-FID2.
             03  W-FID21      PIC  X(006) VALUE SPACE.
             03  W-FID22      PIC  9(003).
             03  W-FID23      PIC  X(003) VALUE SPACE.
           02  I-ID           PIC  X(012) VALUE SPACE.
           02  O-ID           PIC  X(012) VALUE SPACE.
           02  FLCIFO         PIC  X(003) VALUE SPACE.
           02  FLCMOD         PIC  X(003) VALUE SPACE.
           02  FLCOUT1        PIC  X(072) VALUE SPACE.
           02  FLCOUT2        PIC  X(072) VALUE SPACE.
           02  FLCOUT3        PIC  X(072) VALUE SPACE.
           02  FLCOUT4        PIC  X(007) VALUE SPACE.
           02  FLCSEL1        PIC  X(072) VALUE SPACE.
           02  FLCSEL2        PIC  X(048) VALUE SPACE.
      *
       01  AFLCNV-PAR.
           03  F              PIC  X(007) VALUE "#FLCNV;".
           03  F              PIC  X(017) VALUE
                "IDE=MSD_ICI=_IFI=".
           03  A-I-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(010) VALUE
                "_IGN=_IFO=".
           03  A-FLCIFO       PIC  X(003) VALUE SPACE.
           03  F              PIC  X(018) VALUE
                "_ODE=MSD_OCI=_OFI=".
           03  A-O-ID         PIC  X(012) VALUE SPACE.
           03  F              PIC  X(025) VALUE
                "_OGN=_LST=NO_ERR=ABO_MOD=".
           03  A-FLCMOD       PIC  X(003) VALUE SPACE.
           03  F              PIC  X(039) VALUE
                "_AMD=LOG_UMD=NO_GTR=_DEL=NO_CLR=NO_OUT=".
           03  A-FLCOUT1      PIC  X(072) VALUE SPACE.
           03  A-FLCOUT2      PIC  X(072) VALUE SPACE.
           03  A-FLCOUT3      PIC  X(072) VALUE SPACE.
           03  A-FLCOUT4      PIC  X(007) VALUE SPACE.
           03  F              PIC  X(010) VALUE "_RNG=_SEL=".
           03  A-FLCSEL1      PIC  X(072) VALUE SPACE.
           03  A-FLCSEL2      PIC  X(048) VALUE SPACE.
           03  F              PIC  X(011) VALUE "_SAV=_NXT=_".
           03  F              PIC  X(002) VALUE "/>".
       01  PAR-SIZ.
           03  PAR-SIZ01      PIC  9(004) VALUE 512.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-MID1.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
           02  FILLER.
             03  0101C-MID1  PIC  X(040) .
             03  FILLER      PIC  X(011) VALUE "DATE.  /  /".
             03  0301C-MID1  PIC  9(002) .
             03  0401C-MID1  PIC  9(002) .
             03  0501C-MID1  PIC  9(002) .
       01  C-MID2.
           03  FILLER    PIC  X(025) VALUE "*************************".
           03  FILLER    PIC  X(025) VALUE "**                     **".
           03  FILLER    PIC  X(018) VALUE   "  ïœä∑èàóùÅ@é¿çsíÜ".
           03  FILLER    PIC  X(025) VALUE "*************************".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
       PROCEDURE                   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-MID1
       CALL "SD_Init" USING
            "C-MID1" "X" "0" "0" "69" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "00C-MID1" "X" "0" "0" "12" " " "C-MID1"  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID1" " " "1" "0" "57" "00C-MID1" " "  RETURNING RESU.
       CALL "SD_Init" USING
          "0101C-MID1" "RX" "1" "20" "40" " " "01C-MID1" RETURNING RESU.
       CALL "SD_From" USING
            "0101C-MID1" BY REFERENCE K-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
         "0201C-MID1" "X" "1" "68" "11" "0101C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
          "0301C-MID1" "9" "1" "73" "2" "0201C-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
            "0301C-MID1" BY REFERENCE YY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
          "0401C-MID1" "9" "1" "76" "2" "0301C-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
            "0401C-MID1" BY REFERENCE MM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
          "0501C-MID1" "9" "1" "79" "2" "0401C-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
            "0501C-MID1" BY REFERENCE DD "2" "0" RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING
           "C-MID2" " " "0" "0" "93" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID2" "X" "12" "25" "25" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID2" "X" "13" "25" "25" "01C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID2" "X" "13" "27" "18" "02C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID2" "X" "14" "25" "25" "03C-MID2" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "10" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO-2 TO W-FID12 W-FID22.
           ACCEPT HIZUKE FROM DATE.
      *
           ACCEPT W-KBN FROM ARGUMENT-VALUE.
           IF  W-KBN NOT = 01 AND 10 AND 11
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           IF  W-KBN = 01
               ACCEPT I-ID FROM ARGUMENT-VALUE
           END-IF
           IF  W-KBN = 10 OR 11
               ACCEPT W-FID11 FROM ARGUMENT-VALUE
               MOVE W-FID1 TO I-ID
           END-IF
           IF  I-ID = SPACE
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           ACCEPT FLCIFO FROM ARGUMENT-VALUE.
           IF  FLCIFO NOT = "PRO" AND "SHA"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           IF  W-KBN = 01 OR 11
               ACCEPT W-FID21 FROM ARGUMENT-VALUE
               MOVE W-FID2 TO O-ID
           END-IF
           IF  W-KBN = 10
               ACCEPT O-ID FROM ARGUMENT-VALUE
           END-IF
           IF  O-ID = SPACE
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           ACCEPT FLCMOD FROM ARGUMENT-VALUE.
           IF  FLCMOD NOT = "ADD" AND "CRE"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           ACCEPT FLCOUT1 FROM ARGUMENT-VALUE.
           ACCEPT FLCOUT2 FROM ARGUMENT-VALUE.
           ACCEPT FLCOUT3 FROM ARGUMENT-VALUE.
           ACCEPT FLCOUT4 FROM ARGUMENT-VALUE.
      *
           ACCEPT FLCSEL1 FROM ARGUMENT-VALUE.
           ACCEPT FLCSEL2 FROM ARGUMENT-VALUE.
      *
           ACCEPT K-MSG FROM ARGUMENT-VALUE.
      *
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
      *
           MOVE I-ID TO A-I-ID.
           MOVE FLCIFO TO A-FLCIFO.
           MOVE O-ID TO A-O-ID.
           MOVE FLCMOD TO A-FLCMOD.
           MOVE FLCOUT1 TO A-FLCOUT1.
           MOVE FLCOUT2 TO A-FLCOUT2.
           MOVE FLCOUT3 TO A-FLCOUT3.
           MOVE FLCOUT4 TO A-FLCOUT4.
           MOVE FLCSEL1 TO A-FLCSEL1.
           MOVE FLCSEL2 TO A-FLCSEL2.
           CALL "CBLRUN" USING AFLCNV-PAR PAR-SIZ.
           CALL "DB_Close".
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSS22L.
      *********************************************************
      *    PROGRAM         :  納品書発行（東海・ながの）      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *    COMPILE TYPE    :  COBOL                           *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=東海 , 1=ながの , 2=日生ｷｬﾛｯﾄ *
      *                    :  3=とうきょう 4=きんき           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(01).
       77  W-COOP             PIC  N(14).
       77  W-TCD              PIC  9(04).
       77  SNOWK-ID           PIC  X(06)  VALUE  "SNOWK3".
       77  W-FILE             PIC  X(13).
       01  ERR-STAT           PIC  X(02).
       01  HEAD1.
           02  F              PIC  X(05)  VALUE  X"1A24212078".
           02  W-HEAD1.
             03  F            PIC  X(06)  VALUE  SPACE.
             03  H-STE03      PIC  N(14).
             03  F            PIC  X(43)  VALUE  SPACE.
             03  H-JYUSYO     PIC  N(12).
           02  F              PIC  X(05)  VALUE  X"1A24212474".
       01  HEAD2.
           02  F              PIC  X(05)  VALUE  X"1A24212078".
           02  W-HEAD2.
             03  F            PIC  X(06)  VALUE  SPACE.
             03  H-STE04      PIC  N(14).
             03  H-SNO081     PIC  9(04).
             03  H-SNO081D  REDEFINES H-SNO081   PIC  Z999.
             03  H-SNO081K  REDEFINES H-SNO081   PIC  X(04).
             03  H-SNO08V     PIC  X(01)  VALUE  "-".
             03  H-SNO082     PIC  9(05).
             03  H-SNO082D  REDEFINES H-SNO082   PIC  X(05).
             03  F            PIC  X(03)  VALUE  SPACE.
             03  H-STE06      PIC  X(04).
             03  F            PIC  X(01)  VALUE  SPACE.
             03  H-STE07      PIC  X(02).
             03  F            PIC  X(01)  VALUE  SPACE.
             03  H-SNO051     PIC  Z999999999.
             03  H-SNO051D  REDEFINES H-SNO051   PIC  ZZZZ999999.
             03  F            PIC  X(02)  VALUE  SPACE.
             03  H-STE05      PIC  X(06).
             03  F            PIC  X(04)  VALUE  SPACE.
             03  H-TEL        PIC  X(13).
             03  F            PIC  X(08)  VALUE  SPACE.
             03  H-SNO06.
               04  H-SNO061   PIC  ZZ.
               04  H-SNO062   PIC  ZZ.
               04  H-SNO063   PIC  ZZ.
             03  F            PIC  X(01)  VALUE  SPACE.
             03  H-SNO07.
               04  H-SNO071   PIC  Z9.
               04  H-SNO072   PIC  Z9.
               04  H-SNO073   PIC  Z9.
           02  F              PIC  X(05)  VALUE  X"1A24212474".
       01  MEI.
           02  F              PIC  X(05)  VALUE  X"1A24212078".
           02  W-MEI.
             03  F            PIC  X(04)  VALUE  SPACE.
             03  M-SNO09      PIC  N(16).
             03  F            PIC  X(01)  VALUE  SPACE.
             03  M-SNO11      PIC  X(13).
             03  F            PIC  X(15)  VALUE  SPACE.
             03  M-SNO12      PIC  -----.
             03  F            PIC  X(14)  VALUE  SPACE.
             03  M-SNO13      PIC  Z(06).
             03  F            PIC  X(02)  VALUE  SPACE.
             03  M-SNO14      PIC  ---------.
             03  M-SNO15      PIC  Z(06).
             03  M-SNO16      PIC  ---------.
           02  F              PIC  X(05)  VALUE  X"1A24212474".
       01  FOOT.
           02  F              PIC  X(05)  VALUE  X"1A24212078".
           02  W-FOOT.
             03  F            PIC  X(05)  VALUE  SPACE.
             03  F-SNO17      PIC  N(10).
             03  F            PIC  X(37)  VALUE  SPACE.
             03  F-SURYO      PIC  -----.
             03  F            PIC  X(22)  VALUE  SPACE.
             03  F-GENKA      PIC  ---------.
             03  F            PIC  X(06)  VALUE  SPACE.
             03  F-BIKO       PIC  ---------.
           02  F              PIC  X(05)  VALUE  X"1A24212474".
       01  W-SCR.
           02  W-YMD.
             03  W-YEAR       PIC  9(04).
             03  W-MONTH      PIC  9(02).
             03  W-DAY        PIC  9(02).
           02  W-PRNT         PIC  9(01).
           02  W-TEST         PIC  9(01).
           02  W-DDM          PIC  9(01).
           02  W-FROM         PIC  9(06).
           02  W-TO           PIC  9(06).
       01  W-SYSDATE.
           02  W-SYSYMD       PIC  9(08).
           02  F              REDEFINES    W-SYSYMD.
             03  W-SYSY       PIC  9(04).
             03  W-SYSM       PIC  9(02).
             03  W-SYSD       PIC  9(02).
       01  W-DATA.
           02  W-FLG          PIC  9(01).
           02  CNT            PIC  9(02).
           02  W-GNO          PIC  9(01).
           02  W-NNO          PIC  9(10).
           02  W-KIKAKU       PIC  N(23).
           02  W-SURYO        PIC S9(04).
           02  W-GENKA        PIC S9(09).
           02  W-BIKO         PIC S9(09).
       01  W-OLD.
           02  O-TYCD         PIC  9(07).
           02  O-KICD         PIC  9(07).
           02  O-HATNO        PIC  9(06).
           COPY    LWSIZC.
           COPY    LSTAT.
           COPY    LWMSG.
      *
           COPY    LSKHAT.
           COPY    LSTENM.
           COPY    LSNOWK.
           COPY    LIHIM2.
      *FD  SP-F
       77  SP-R               PIC  X(206).
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
       01  ACP-AREA.
           02  FILLER.
             03  A-YEAR      PIC  9(02).
             03  A-MONTH     PIC  9(02).
             03  A-DAY       PIC  9(02).
           02  A-PRNT     PIC  9(01).
           02  A-TEST     PIC  9(01).
           02  A-DDM      PIC  9(01).
           02  A-FROM     PIC  9(06).
           02  A-TO       PIC  9(06).
       01  CLR-AREA.
           02  C-FROM  PIC Z(06).
           02  C-TO    PIC Z(06).
       01  DSP-AREA.
           02  FILLER.
             03  D-TITLE     PIC  N(19)  VALUE
                 "＊＊＊　　生協　納品書　発行　　＊＊＊".
             03  D-SYSYMD    PIC  N(05)  VALUE
                 "年　月　日".
             03  D-SYSY      PIC  9(02).
             03  D-SYSM      PIC  9(02).
             03  D-SYSD      PIC  9(02).
           02  D-COPE     PIC  N(14).
           02  FILLER.
             03  D-YMD       PIC  X(22)  VALUE
               "納  期 ：   年  月  日".
           02  D-PRNT     PIC  N(05)  VALUE
             "１　発　行".
           02  FILLER.
             03  D-REPRNT    PIC  N(05)  VALUE
               "２　再発行".
             03  D-SEL       PIC  X(08)  VALUE
                 "選択 [ ]".
           02  D-TEST     PIC  N(05)  VALUE
             "テスト印字".
           02  D-YN       PIC  X(18)  VALUE
               "(YES=1,NO=2)   [ ]".
           02  D-FT.
               03  FILLER    PIC  X(06)  VALUE "発注№".
               03  FILLER    PIC  X(08)  VALUE "ＦＲＯＭ".
               03  FILLER    PIC  X(04)  VALUE "ＴＯ".
           02  D-DDM      PIC  X(21)  VALUE
                 "確認 OK=1 NO=9   ﾘﾀｰﾝ".
       01  MSG-AREA.
           02  M-ERR01    PIC  N(09)  VALUE
               "印字対象データなし".
           02  M-ERR11    PIC  X(28)  VALUE
               "社店マスタなし    続行 : ESC".
           COPY    LSSEM.
           COPY    LSMSG.
      *
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
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "21" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "6" "0" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YEAR" "9" "6" "31" "2" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YEAR" BY REFERENCE W-YEAR "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MONTH" "9" "6" "35" "2" "A-YEAR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MONTH" BY REFERENCE W-MONTH "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DAY" "9" "6" "39" "2" "A-MONTH" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DAY" BY REFERENCE W-DAY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PRNT" "9" "9" "42" "1" "01ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PRNT" BY REFERENCE W-PRNT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TEST" "9" "12" "42" "1" "A-PRNT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TEST" BY REFERENCE W-TEST "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DDM" "9" "23" "66" "1" "A-TEST" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DDM" BY REFERENCE W-DDM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FROM" "9" "15" "31" "6" "A-DDM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FROM" BY REFERENCE W-FROM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TO" "9" "16" "31" "6" "A-FROM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TO" BY REFERENCE W-TO "6" "0" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-FROM" "Z" "15" "31" "6" " " "CLR-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "C-FROM" BY REFERENCE W-FROM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-TO" "Z" "16" "31" "6" "C-FROM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-TO" BY REFERENCE W-TO "6" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "199" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "54" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TITLE" "N" "1" "16" "38" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SYSYMD" "N" "1" "63" "10" "D-TITLE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SYSY" "9" "1" "61" "2" "D-SYSYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SYSY" BY REFERENCE W-SYSY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SYSM" "9" "1" "65" "2" "D-SYSY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SYSM" BY REFERENCE W-SYSM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SYSD" "9" "1" "69" "2" "D-SYSM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SYSD" BY REFERENCE W-SYSD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-COPE" "N" "3" "19" "28" "01DSP-AREA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-COPE" BY REFERENCE W-COOP "28" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" " " "6" "0" "22" "D-COPE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YMD" "X" "6" "21" "22" " " "03DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRNT" "N" "8" "21" "10" "03DSP-AREA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" " " "9" "0" "18" "D-PRNT" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "D-REPRNT" "N" "9" "21" "10" " " "05DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SEL" "X" "9" "36" "8" "D-REPRNT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TEST" "N" "11" "21" "10" "05DSP-AREA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YN" "X" "12" "26" "18" "D-TEST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FT" " " "0" "0" "18" "D-YN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-FT" "X" "14" "31" "6" " " "D-FT" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-FT" "X" "15" "21" "8" "01D-FT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-FT" "X" "16" "21" "4" "02D-FT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DDM" "X" "23" "50" "21" "D-FT" " " RETURNING RESU.
      *MSG-AREA
       CALL "SD_Init" USING 
            "MSG-AREA" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "M-ERR01" "N" "24" "15" "18" " " "MSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "M-ERR11" "X" "24" "15" "28" "M-ERR01" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAIN-01.
           ACCEPT     JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN     =  0
               MOVE  "【東海コープ　　　　　　　】" TO  W-COOP
               MOVE  3245    TO  W-TCD
           ELSE
               IF  JS-SIGN     =  1
                   MOVE  "【ながのコープ　　　　　　】" TO  W-COOP
                   MOVE  3248    TO  W-TCD
               ELSE
                   IF  JS-SIGN     =  2
                       MOVE  "【日生協キャロット　　　　】" TO  W-COOP
                       MOVE  5366    TO  W-TCD
                   ELSE
                       IF  JS-SIGN     =  3
                         MOVE  "【コープとうきょう　　　　】" TO  W-COOP
                         MOVE  3241    TO  W-TCD
                       ELSE
                         IF  JS-SIGN     =  4
                         MOVE  "【コープきんき　　　　　　】" TO  W-COOP
                         MOVE  3246    TO  W-TCD
                         ELSE
                         IF  JS-SIGN     =  9
                         MOVE  "【主婦の友ダイレクト　　　】" TO  W-COOP
                         MOVE  2820    TO  W-TCD
                         ELSE
                         CALL "DB_Close"
                         STOP  RUN
                         END-IF
                         END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           PERFORM    INIT-RTN  THRU  INIT-EX.
       MAIN-02.
           PERFORM    ACP-RTN  THRU  ACP-EX.
           IF  W-FLG = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           ELSE
               PERFORM    LST-RTN  THRU  LST-EX
               IF  W-FLG = 5
                   MOVE     0  TO  W-FLG
                   GO TO    MAIN-02
               END-IF
           END-IF.
       MAIN-05.
           PERFORM    END-RTN  THRU  END-EX.
           CALL "DB_Close".
           STOP RUN.
      *
      *-----初期処理-----*
       INIT-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           ACCEPT     W-SYSYMD  FROM  DATE.
           ADD     2000  TO  W-SYSY.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           INITIALIZE    W-DATA  W-SCR.
           MOVE    1   TO  W-PRNT  W-TEST.
       INIT-EX.
           EXIT.
      *
      *-----終了処理-----*
       END-RTN.
           IF  W-FLG NOT = 9
               CALL "DB_F_Close" USING
                BY REFERENCE SK-HAT_IDLST SK-HAT_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE STENM_IDLST STENM_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE SNOWK_IDLST SNOWK_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI2-M_IDLST HI2-M_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      *
      *-----画面処理-----*
       ACP-RTN.
       ACP-01.
           INITIALIZE    W-YEAR.
           CALL "SD_Accept" USING BY REFERENCE A-YEAR "A-YEAR" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               MOVE     9  TO  W-FLG
               GO TO    ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND NOC
               GO TO    ACP-01
           END-IF
           ADD    2000  TO  W-YEAR.
       ACP-02.
           CALL "SD_Accept" USING BY REFERENCE A-MONTH "A-MONTH" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-01
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND NOC
               GO TO    ACP-02
           END-IF
           IF  W-MONTH > 12
               GO TO    ACP-02
           END-IF.
       ACP-03.
           CALL "SD_Accept" USING BY REFERENCE A-DAY "A-DAY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-02
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-03
           END-IF
           IF  W-YMD = "20000000"
               MOVE    W-SYSYMD  TO  W-YMD
               CALL "SD_Output" USING "A-YEAR" A-YEAR "p" RETURNING RESU
               CALL "SD_Output" USING
                "A-MONTH" A-MONTH "p" RETURNING RESU
               CALL "SD_Output" USING "A-DAY" A-DAY "p" RETURNING RESU
           END-IF
           IF  W-DAY = 00
               GO TO    ACP-03
           END-IF
           IF  W-MONTH = 02
               IF  W-DAY > 29
                   GO TO    ACP-03
               ELSE
                   GO TO    ACP-04
               END-IF
           END-IF
           IF  W-MONTH = 04 OR 06 OR 09 OR 11
               IF  W-DAY > 30
                   GO TO    ACP-03
               ELSE
                   GO TO    ACP-04
               END-IF
           END-IF
           IF  W-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
               IF  W-DAY > 31
                   GO TO    ACP-03
               END-IF
           ELSE
               GO TO    ACP-02
           END-IF.
       ACP-04.
           CALL "SD_Output" USING "A-PRNT" A-PRNT "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-PRNT "A-PRNT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-03
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-04
           END-IF
           IF  W-PRNT NOT = 1 AND 2
               GO TO    ACP-04
           END-IF.
       ACP-05.
           CALL "SD_Output" USING "A-TEST" A-TEST "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TEST "A-TEST" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-04
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-05
           END-IF
           IF  W-TEST NOT = 1 AND 2
               GO TO    ACP-05
           END-IF
           IF  W-TEST = 1
               PERFORM    TEST-RTN  THRU  TEST-EX
               GO TO    ACP-05
           END-IF.
       ACP-06.
           IF  W-PRNT NOT = 2
               MOVE  ZERO     TO W-FROM W-TO
               CALL "SD_Output" USING "C-FROM" C-FROM "p" RETURNING RESU
               CALL "SD_Output" USING "C-TO" C-TO "p" RETURNING RESU
               GO  TO  ACP-99
           END-IF
      *
           CALL "SD_Accept" USING BY REFERENCE A-FROM "A-FROM" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-05
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-06
           END-IF.
       ACP-07.
           CALL "SD_Accept" USING BY REFERENCE A-TO "A-TO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-06
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-07
           END-IF
           IF  W-FROM > W-TO
               GO TO    ACP-07
           END-IF.
       ACP-99.
           CALL "SD_Accept" USING BY REFERENCE A-DDM "A-DDM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-PRNT NOT = 2
                   GO TO    ACP-05
               ELSE
                   GO TO    ACP-07
               END-IF
           END-IF
           IF  ESTAT = PF9
               MOVE     9  TO  W-FLG
               GO TO    ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-99
           END-IF
           IF  W-DDM = 9
               MOVE     9  TO  W-FLG
           ELSE
               IF  W-DDM NOT = 1
                   GO TO    ACP-99
               END-IF
           END-IF.
       ACP-EX.
           EXIT.
      *
      *----出力処理-----*
       LST-RTN.
           CALL "DB_F_Open" USING
            "I-O" SK-HAT_PNAME1 "SHARED" BY REFERENCE SK-HAT_IDLST "1"
            "HAT-KEY" BY REFERENCE HAT-KEY.
           CALL "DB_F_Open" USING
            "INPUT" STENM_PNAME1 "SHARED" BY REFERENCE STENM_IDLST "1"
            "STE-KEY1" BY REFERENCE STE-KEY1.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE SNOWK-ID TO SNOWK_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SNOWK_PNAME1 " " BY REFERENCE SNOWK_IDLST "0".
           MOVE    0  TO  W-NNO  W-GNO.
           IF  W-PRNT = 1
               MOVE    0         TO  W-FLG
               MOVE    ZERO      TO  W-FROM
               MOVE    999999    TO  W-TO
           ELSE
               MOVE    1  TO  W-FLG
           END-IF
      *           SELECT    SK-HAT
      *               WHERE    HAT-041 = W-TCD AND HAT-03 = W-YMD
      *                    AND HAT-99  = 0    AND HAT-97 = W-FLG
      *                    AND HAT-01  NOT <  W-FROM
      *                    AND HAT-01  NOT >  W-TO
      *               ORDER BY    HAT-04 HAT-18 HAT-01 HAT-02.
      *///////////////
           CALL "DB_Select" USING
            SK-HAT_PNAME1 "WHERE" 
            "HAT-041" "=" W-TCD "AND"
            "HAT-03" "=" W-YMD "AND"
            "HAT-99" "=" "0" "AND"
            "HAT-97" "=" W-FLG "AND"
            "HAT-01" "NOT <" W-FROM "AND"
            "HAT-01" "NOT >" W-TO
            "ORDER BY" "HAT-04" "HAT-18" "HAT-01" "HAT-02"
            RETURNING RET.
           MOVE    5  TO  W-FLG.
       LST-01.
      *           READ    SK-HAT    NEXT RECORD  WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SK-HAT_PNAME1 BY REFERENCE HAT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING SK-HAT_PNAME1
               GO TO    LST-02
           END-IF
           IF  HAT-11 = 0
               GO TO    LST-01
           END-IF
           MOVE    HAT-07  TO  SIZE-WK-HIN.
           MOVE    HAT-08  TO  SIZE-WK-CD.
           PERFORM    SIZE-RTN  THRU  SIZE-EX.
           IF  SIZE-WK-SW NOT = 0
               GO TO    LST-01
           END-IF
           MOVE    0  TO  W-FLG.
           PERFORM    WSNOWK-RTN  THRU  WSNOWK-EX.
           GO TO    LST-01.
       LST-02.
           CALL "DB_F_Close" USING
            BY REFERENCE SNOWK_IDLST SNOWK_PNAME1.
           IF  W-FLG = 5
               CALL "SD_Output" USING
                "M-ERR01" M-ERR01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE SK-HAT_IDLST SK-HAT_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE STENM_IDLST STENM_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI2-M_IDLST HI2-M_PNAME1
               GO TO    LST-EX
           END-IF
           MOVE SNOWK-ID TO SNOWK_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNOWK_PNAME1 " " BY REFERENCE SNOWK_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
      *           READ    SNOWK    NEXT RECORD  WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SNOWK_PNAME1 BY REFERENCE SNO-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO    LST-99
           END-IF
           MOVE    99  TO  CNT.
           MOVE    SNO-03  TO  O-HATNO.
           MOVE    SNO-04  TO  W-GNO.
           PERFORM    HEAD-RTN  THRU  HEAD-EX.
       LST-03.
           INITIALIZE    W-MEI.
           MOVE     SNO-091  TO  M-SNO09.
           MOVE     SPACE    TO  SP-R.
           MOVE     MEI      TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE     W-MEI.
           MOVE     SNO-092  TO  M-SNO09.
           MOVE     SNO-10   TO  M-SNO09(11:04).
           MOVE     SNO-11   TO  M-SNO11.
           MOVE     SNO-12   TO  M-SNO12.
           MOVE     SNO-13   TO  M-SNO13.
           MOVE     SNO-14   TO  M-SNO14.
           MOVE     SNO-15   TO  M-SNO15.
           MOVE     SNO-16   TO  M-SNO16.
           MOVE     SPACE    TO  SP-R.
           MOVE     MEI      TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD    2  TO  CNT.
           ADD    SNO-12  TO  W-SURYO.
           ADD    SNO-14  TO  W-GENKA.
           ADD    SNO-16  TO  W-BIKO.
      *           READ    SNOWK  NEXT RECORD  WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SNOWK_PNAME1 BY REFERENCE SNO-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               PERFORM    RHAT-RTN  THRU  RHAT-EX
               PERFORM    FOOT-RTN  THRU  FOOT-EX
               GO TO    LST-99
           END-IF
           IF  W-PRNT = 1
               IF  NOT(SNO-03 = O-HATNO AND SNO-04 = W-GNO)
                   PERFORM    RHAT-RTN  THRU  RHAT-EX
               END-IF
           END-IF
           IF  O-HATNO    = SNO-03
               MOVE    SNO-04  TO  W-GNO
               GO TO    LST-03
           ELSE
               PERFORM    FOOT-RTN  THRU  FOOT-EX
               PERFORM    HEAD-RTN  THRU  HEAD-EX
               MOVE    SNO-03  TO  O-HATNO
               MOVE    SNO-04  TO  W-GNO
               GO TO    LST-03
           END-IF.
       LST-99.
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      *
      *-----ヘッダー印字処理-----*
       HEAD-RTN.
           IF  CNT NOT = 99
               MOVE     SPACE  TO  SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE    SNO-051  TO  W-NNO.
           MOVE    SNO-17   TO  W-KIKAKU.
           MOVE    SNO-08   TO  STE-02.
      *           READ    STENM    WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" STENM_PNAME1 BY REFERENCE STE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "M-ERR11" M-ERR11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               INITIALIZE    STE-R
           END-IF
           INITIALIZE    W-HEAD1.
           MOVE     STE-03(01:13)   TO  H-STE03.
           MOVE     "岡山市今８丁目１６－１７"  TO  H-JYUSYO.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD1    TO  SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE    W-HEAD1.
           MOVE     "日進ゴム株式会社　　　　"  TO  H-JYUSYO.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD1    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     STE-04(01:13)   TO  H-STE04.
           IF  JS-SIGN NOT =  3
               MOVE     SNO-081  TO  H-SNO081
               MOVE     SNO-082  TO  H-SNO082
               MOVE     SNO-051  TO  H-SNO051
           ELSE
               MOVE     SNO-081  TO  H-SNO081D
               MOVE     SPACE    TO  H-SNO08V  H-SNO082D
               MOVE     SNO-051  TO  H-SNO051D
           END-IF
           MOVE     STE-06   TO  H-STE06.
           MOVE     STE-07   TO  H-STE07.
           IF  JS-SIGN     =  9
               MOVE     SPACE    TO  H-SNO081K  H-SNO08V  H-SNO082D
                                     H-STE06    H-STE07
           END-IF
           MOVE     STE-05   TO  H-STE05.
           MOVE     "086-243-2456"   TO  H-TEL.
           MOVE     SNO-061(03:02)   TO  H-SNO061.
           MOVE     SNO-062  TO  H-SNO062.
           MOVE     SNO-063  TO  H-SNO063.
           MOVE     SNO-071(03:02)   TO  H-SNO071.
           MOVE     SNO-072  TO  H-SNO072.
           MOVE     SNO-073  TO  H-SNO073.
           MOVE     SPACE    TO  SP-R.
           MOVE     HEAD2    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE    TO  SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     ZERO     TO  W-SURYO  W-GENKA  W-BIKO  CNT.
       HEAD-EX.
           EXIT.
      *
      *-----フッター印字処理-----*
       FOOT-RTN.
           MOVE     SPACE  TO  SP-R.
           COMPUTE    CNT = 13 - CNT.
           CALL "PR_LineFeed" USING CNT RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE    W-FOOT.
           MOVE     W-SURYO  TO  F-SURYO.
           MOVE     W-GENKA  TO  F-GENKA.
           MOVE     W-BIKO   TO  F-BIKO.
           MOVE     SPACE    TO  SP-R.
           MOVE     FOOT     TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE    W-FOOT.
           MOVE     W-KIKAKU(01:10)  TO  F-SNO17.
           MOVE     SPACE  TO  SP-R.
           MOVE     FOOT   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE    W-FOOT.
           MOVE     W-KIKAKU(11:10)  TO  F-SNO17.
           MOVE     SPACE  TO  SP-R.
           MOVE     FOOT   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE    W-FOOT.
           MOVE     W-KIKAKU(21:03)  TO  F-SNO17.
           MOVE     SPACE  TO  SP-R.
           MOVE     FOOT   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE  TO  SP-R.
       FOOT-EX.
           EXIT.
      *
      *-----テスト印字処理-----*
       TEST-RTN.
           CALL "PR_Open" RETURNING RESP.
           INITIALIZE     W-HEAD1  W-HEAD2.
           MOVE     ALL "X"  TO  H-STE03(01:13)  H-JYUSYO.
           MOVE     SPACE  TO  SP-R.
           MOVE     HEAD1  TO  SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE      H-STE03.
           MOVE     SPACE  TO  SP-R.
           MOVE     HEAD1  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     ALL "X"  TO  H-STE06   H-TEL     H-STE04(01:13).
           MOVE     ALL "9"  TO  H-SNO081  H-SNO082  H-STE07
                                 H-STE05   H-SNO06   H-SNO07.
           MOVE     SPACE  TO  SP-R.
           MOVE     HEAD2  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE  TO  SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     1  TO  CNT.
       TEST-01.
           INITIALIZE    W-MEI.
           MOVE     ALL "X"  TO  M-SNO09.
           MOVE     SPACE  TO  SP-R.
           MOVE     MEI    TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE    W-MEI.
           MOVE     ALL "X"  TO  M-SNO09  M-SNO11.
           MOVE     ALL "9"  TO  M-SNO12  M-SNO13  M-SNO14
                                 M-SNO15  M-SNO16.
           MOVE     SPACE    TO  SP-R.
           MOVE     MEI      TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  CNT < 6
               ADD    1  TO  CNT
               GO TO     TEST-01
           END-IF
           INITIALIZE    W-FOOT.
           MOVE     ALL "9"  TO  F-SURYO  F-GENKA  F-BIKO.
           MOVE     SPACE  TO  SP-R.
           MOVE     FOOT   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           INITIALIZE    W-FOOT.
           MOVE     ALL "X"  TO  F-SNO17.
           MOVE     1  TO  CNT.
       TEST-02.
           MOVE     SPACE  TO  SP-R.
           MOVE     FOOT   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           IF  CNT < 3
               ADD    1  TO  CNT
               GO TO    TEST-02
           END-IF
           MOVE     SPACE  TO  SP-R.
           CALL "PR_Close" RETURNING RESP.
       TEST-EX.
           EXIT.
      *
      *-----生協納品書ワーク３追加処理-----*
       WSNOWK-RTN.
           MOVE    HAT-04  TO  SNO-01.
           MOVE    HAT-18  TO  SNO-02.
           MOVE    HAT-01  TO  SNO-03.
           MOVE    HAT-02  TO  SNO-04  SNO-052.
           MOVE    HAT-23  TO  SNO-051.
           MOVE    HAT-10  TO  SNO-06.
           MOVE    HAT-03  TO  SNO-07.
           MOVE    HAT-06  TO  SNO-08.
           MOVE    HAT-09  TO  SNO-09.
           MOVE    SIZE-WK-NM  TO  SNO-10.
           MOVE    HAT-15  TO  SNO-11.
           MOVE    HAT-13  TO  SNO-13.
           MOVE    HAT-14  TO  SNO-15.
           MOVE    HAT-19  TO  SNO-17.
           MOVE    HAT-15  TO  SNO-11.
           MOVE    HAT-24  TO  SNO-18.
           IF  HAT-24 = 0
               MOVE    HAT-11  TO  SNO-12
           ELSE
               COMPUTE    SNO-12 = HAT-11 * (-1)
           END-IF
           COMPUTE    SNO-14 = SNO-13 * SNO-12.
           COMPUTE    SNO-16 = SNO-15 * SNO-12.
      *           WRITE    SNO-R.
      *//////////////
           CALL "DB_Insert" USING
            SNOWK_PNAME1 SNOWK_LNAME SNO-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE    "W"       TO  ERR-M
               MOVE    "SNOWK"   TO  ERR-F
               MOVE    SNO-R     TO  ERR-K
               PERFORM    ERR-RTN  THRU  ERR-EX
           END-IF.
       WSNOWK-EX.
           EXIT.
      *
      *-----生協　発注データ修正処理-----*
       RHAT-RTN.
           MOVE    O-HATNO  TO  HAT-01.
           MOVE    W-GNO    TO  HAT-02.
      *           READ    SK-HAT   INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SK-HAT_PNAME1 BY REFERENCE HAT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO    RHAT-EX
           END-IF
           MOVE    1  TO  HAT-97.
      *           REWRITE    HAT-R  INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SK-HAT_PNAME1 SK-HAT_LNAME HAT-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"        TO  ERR-M
               MOVE    "SK-HAT"   TO  ERR-F
               MOVE    HAT-KEY    TO  ERR-K
               PERFORM    ERR-RTN  THRU  ERR-EX
           END-IF.
       RHAT-EX.
           EXIT.
      *
      ***
           COPY    LPSIZC.
           COPY    LPMSG.

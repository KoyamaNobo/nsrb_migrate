       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PRG050.
      *********************************************
      *    財務残高表                             *
      *    DATE  :  92/11/25                      *
      *********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  W-20K               PIC X(5)    VALUE X"1A24212474".
       01  HEAD1.
           02  W-15K           PIC X(5)    VALUE X"1A24212078".
           02  FILLER          PIC X(34)   VALUE SPACE.
           02  FILLER          PIC N(12)   VALUE
                               "【　　残高　明細表　　】".
           02  FILLER          PIC X(19)   VALUE SPACE.
           02  FILLER          PIC X(02)   VALUE "P.".
           02  H-PAGE          PIC Z9.
       01  HEAD2.
           02  FILLER          PIC X(33)   VALUE SPACE.
           02  FILLER          PIC X(01)   VALUE "(".
           02  H-DATE          PIC N(12).
           02  FILLER          PIC X(01)   VALUE ")".
           02  FILLER          PIC X(22)   VALUE SPACE.
       01  HEAD3.
           02  FILLER          PIC X(14)   VALUE SPACE.
           02  FILLER          PIC X(07)   VALUE "ｺｰﾄﾞ   ".
           02  FILLER          PIC N(06)   VALUE "名　　　称　".
           02  FILLER          PIC X(39)   VALUE SPACE.
           02  FILLER          PIC N(04)   VALUE "金　　額".
       01  W-P1.
           02  FILLER          PIC X(14).
           02  P1-15K          PIC X(05).
           02  P1-KAK          PIC 9(04).
           02  F               PIC X(03).
           02  P1-KNGNM        PIC N(10).
           02  F               PIC X(25).
           02  P1-KIN          PIC --,---,---,--9.
           02  P1-20K          PIC X(05).
       01  W-P2.
           02  FILLER          PIC X(20).
           02  P2-15K          PIC X(05).
           02  P2-F            PIC X(01).
           02  P2-KAK1         PIC 9(04).
           02  P2-V            PIC X(01).
           02  P2-HOK1         PIC 9(04).
           02  P2-R            PIC X(01).
           02  F               PIC X(01).
           02  P2-KNGNM        PIC N(10).
           02  F               PIC X(14).
           02  P2-KIN          PIC --,---,---,--9.
           02  P2-20K          PIC X(05).
       01  W-P3.
           02  FILLER          PIC X(29).
           02  P3-15K          PIC X(05).
           02  P3-F            PIC X(01).
           02  P3-KAK2         PIC 9(04).
           02  P3-V            PIC X(01).
           02  P3-HOK2         PIC 9(04).
           02  P3-R            PIC X(01).
           02  F               PIC X(01).
           02  P3-KNGNM        PIC N(10).
           02  F               PIC X(05).
           02  P3-KIN          PIC --,---,---,--9.
           02  P3-20K          PIC X(05).
       01  W-MR.
           02  W-MZAN          PIC  S9(10).
           02  W-MTJ.
             03  W-MTJIS       OCCURS  15.
               04  W-MTJKR     PIC  S9(11).
               04  W-MTJKS     PIC  S9(11).
       01  W-DATA.
           02  W-DATEM         PIC N(12).
           02  W-NGPM.
             03  W-NENM        PIC N(02).
             03  F             PIC N(01)  VALUE "年".
             03  W-GETM        PIC N(02).
             03  F             PIC N(01)  VALUE "月".
             03  W-PEYM        PIC N(02).
             03  F             PIC N(04)  VALUE "日　現在".
           02  W-NGP.
             03  W-GET         PIC Z(2).
             03  W-PEY         PIC Z(2).
           02  W-KAK           PIC 9(4).
           02  W-KEYD.
             03  W-KAKD        PIC 9(4).
             03  W-HOKD        PIC 9(4).
           02  W-KEYDD         PIC 9(8).
           02  PCNT            PIC 9(2)    VALUE  ZERO.
           02  DCNT            PIC 9(2).
           02  W-TPC           PIC X(1).
           02  W-KAKU          PIC X(1).
           02  W-ED            PIC 9(1)    VALUE  0.
           02  W-POC           PIC 9(1)    VALUE  0.
           02  SET-WORK.
             03  W-ZENKI       PIC S9(10).
             03  W-ZAN         PIC S9(10).
             03  W-KARI        PIC S9(11).
             03  W-KASI        PIC S9(11).
           02  SOE             PIC  9(02).
           02  FI              PIC  9(02).
           02  TI              PIC  9(02).
           02  W-DCD           PIC  9(01).
           02  W-DC            PIC  9(01).
           02  W-MC            PIC  9(01).
           02  W-NO            PIC  9(02).
           02  W-AZAN          PIC S9(10).
           02  W-SZAN          PIC S9(10).
           02  CHK.
             03  CHK1          PIC  9(01).
             03  CHK2          PIC  9(01).
           02  W-SC            PIC  9(01).
      *
      *
       COPY    LWMSG_PR.
       COPY    ACCUNT.
       COPY    LKAZAN.
       COPY    LHOZAN.
       COPY    KANGEL.
       COPY    FCTL.
       01  ZAN-K_PRG050.
           02  ZAN-K_PNAME1    PIC  X(005)  VALUE "ZAN-K".
           02  F               PIC  X(001).
           02  ZAN-K_LNAME     PIC  X(012)  VALUE "ZAN-K_PRG050".
           02  F               PIC  X(001).
           02  ZAN-K_KEY1      PIC  X(100)  VALUE SPACE.
           02  ZAN-K_KEY2      PIC  X(100)  VALUE SPACE.
           02  ZAN-K_SORT      PIC  X(100)  VALUE SPACE.
           02  ZAN-K_IDLST     PIC  X(100)  VALUE SPACE.
           02  ZAN-K_RES       USAGE  POINTER.
       01  ZAN-R.
           02  ZAN-KEY.
             03  ZAN-NO        PIC 9(02).
             03  ZAN-KAK       PIC 9(04).
             03  ZAN-DATA1.
               04  ZAN-KAK1    PIC 9(04).
               04  ZAN-HOK1    PIC 9(04).
             03  ZAN-DATA2.
               04  ZAN-KAK2    PIC 9(04).
               04  ZAN-HOK2    PIC 9(04).
           02  F               PIC X(10).
       77  F                   PIC  X(001).
       77  F5-REC              PIC  X(200).
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER       PIC  9(003).
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RESP                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
       77  ERR-STAT            PIC X(02).
       01  DSP-CLR.
           02  FILLER  PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER.
               03  FILLER         PIC  N(01)  VALUE  "年".
               03  FILLER         PIC  N(02)  VALUE  "月度".
               03  0301DSP-AREA   PIC  9(02).
               03  0401DSP-AREA   PIC  9(02).
           02  FILLER  PIC X(14)   VALUE  "  残高明細表  ".
           02  FILLER  PIC X(24)  VALUE  "０９ X １１  白紙 セット".
           02  FILLER  PIC X(27)  VALUE  "TEST PRINT  ｽﾙ=9 ｼﾅｲ=1 ... ".
           02  FILLER  PIC X(18)  VALUE  "確認 OK=1,NO=9 ( )".
       01  ACP-AREA.
           02  ACP-TPC         PIC 9(01).
           02  ACP-KAKU        PIC X(01).
      *
       01  DSP-ERR.
           02  FILLER.
             03  ERR-MSG1      PIC  X(11) VALUE
                                 "KAMOKU-K ﾅｼ".
             03  ERR-KEY       PIC  9(04).
      *
           COPY  LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "SPOUT1-999-FPG010" RETURNING RESP.
      *           01  DSP-CLR.
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *           01  DSP-AREA.
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "93" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "10" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-AREA" "N" "1" "4" "2" " " "01DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-AREA" "N" "1" "8" "4" "0101DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "9" "1" "2" "2" "0201DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301DSP-AREA" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-AREA" "9" "1" "6" "2" "0301DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401DSP-AREA" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "RX" "1" "33" "14" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "10" "40" "24" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "15" "40" "27" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "24" "61" "18" "04DSP-AREA" " "
            RETURNING RESU.
      *           01  ACP-AREA.
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TPC" "9" "15" "66" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" "X" "24" "77" "1" "ACP-TPC" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *           01  DSP-ERR.
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "15" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ERR" " " "24" "0" "15" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
           "ERR-MSG1" "X" "24" "15" "11" " " "01DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-KEY" "9" "24" "40" "4" "ERR-MSG1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ERR-KEY" BY REFERENCE ZAN-KAK "4" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
      *
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" ZAN-K_PNAME1 
            "SHARED" BY REFERENCE ZAN-K_IDLST "1"
            "ZAN-KEY" BY REFERENCE ZAN-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KZM-F_PNAME1 "SHARED" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HZM-F_PNAME1 "SHARED" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "PR_Open" RETURNING RESP.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE   "DATE  "    TO   FCTL-KEY1.
      *           READ  FCTL-F  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-MCT" INV-MCT "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               GO  TO  END-99
           END-IF.
           MOVE  FCTL-REC       TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           MOVE  Z-KONMM     TO  W-GET.
           MOVE  Z-KONDD     TO  W-PEY.
           MOVE  Z-KONYY2    TO  W-NENM.
           MOVE  W-GET       TO  W-GETM.
           MOVE  W-PEY       TO  W-PEYM.
           MOVE  W-NGPM      TO  W-DATEM.
           MOVE  W-DATEM     TO  H-DATE.
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           PERFORM TEST-RTN  THRU  TEST-EX.
           IF  W-POC        =      5
               MOVE     0            TO     W-POC
               MOVE     SPACE        TO     F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE     ZERO         TO     PCNT
           END-IF.
       ST-05.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               GO  TO  END-99
           END-IF.
           IF  W-KAKU NOT = "1" AND "9"
               GO  TO  ST-05
           END-IF.
           IF  ESTAT NOT = "01"
               GO  TO  ST-05
           END-IF.
           IF  W-KAKU = "9"
               GO  TO  END-99
           END-IF.
      *
           MOVE  Z-KONYMD     TO  ZYMD.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               GO TO END-99
           END-IF.
           MOVE ZI     TO TI.
           IF  TI > 12
               MOVE 13     TO FI
           ELSE
               IF  Z-KSMM = 12
                   MOVE 1     TO FI
               ELSE
                   COMPUTE FI = Z-KSMM + 1
               END-IF
           END-IF.
      *
       ST-10.
      *           READ ZAN-K NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" ZAN-K_PNAME1 BY REFERENCE ZAN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO END-99
           END-IF.
           MOVE ZERO     TO SET-WORK.
           IF  ZAN-KAK2   =  ZERO
               MOVE 1        TO W-DCD
               MOVE ZAN-DATA1 TO W-KEYD
               MOVE W-KAKD   TO AM-KEY
           ELSE
               MOVE 2        TO W-DCD
               MOVE ZAN-DATA2 TO W-KEYD
               MOVE W-KAKD    TO AM-KEY
           END-IF.
      *           READ AM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-MSG1" ERR-MSG1 "p"
                                        RETURNING RESU
               CALL "SD_Output" USING "ERR-KEY" ERR-KEY "p"
                                        RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                        RETURNING RESU
               CALL "SD_Output" USING "DISP-MSG-SPACE"
                DISP-MSG-SPACE "p"RETURNING RESU
               GO  TO  ST-10
           END-IF.
           IF  W-HOKD = ZERO
               PERFORM  MR1-SET-RTN  THRU  MR1-SET-EX
           ELSE
               PERFORM  MR2-SET-RTN  THRU  MR2-SET-EX
           END-IF.
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
           IF  W-ZAN = ZERO
               GO  TO  ST-10
           END-IF.
           MOVE 1        TO W-ED.
       ST-20.
           MOVE ZAN-NO   TO W-NO.
           MOVE ZERO     TO W-MC.
       ST-30.
           MOVE ZAN-KAK  TO W-KAK.
           MOVE ZERO     TO CHK   W-AZAN.
           MOVE ZERO     TO KNG-KEY.
           MOVE W-KAK    TO K-ACCD.
      *           READ       KNG   UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE        TO     KNGNMN
           END-IF.
           MOVE SPACE    TO W-P1.
           MOVE W-15K    TO P1-15K.
           MOVE W-20K    TO P1-20K.
           MOVE W-KAK    TO P1-KAK.
           MOVE KNGNMN   TO P1-KNGNM.
       ST-40.
           MOVE ZERO     TO CHK2   W-SC    W-SZAN.
           MOVE ZAN-DATA1  TO KNG-KEY.
      *           READ       KNG   UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE        TO     KNGNMN
           END-IF.
           MOVE SPACE    TO W-P2.
           MOVE W-15K    TO P2-15K.
           MOVE W-20K    TO P2-20K.
           MOVE "("      TO P2-F.
           MOVE ")"      TO P2-R.
           MOVE ZAN-KAK1 TO P2-KAK1.
           MOVE "-"      TO P2-V.
           MOVE ZAN-HOK1 TO P2-HOK1.
           MOVE KNGNMN   TO P2-KNGNM.
           IF  W-DCD     =   1
               MOVE W-ZAN     TO  P2-KIN
               GO   TO    ST-60
           END-IF.
       ST-50.
           MOVE ZAN-DATA2  TO KNG-KEY.
      *           READ       KNG   UNLOCK    INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE        TO     KNGNMN
           END-IF.
           MOVE SPACE    TO W-P3.
           MOVE W-15K    TO P3-15K.
           MOVE W-20K    TO P3-20K.
           MOVE "("      TO P3-F.
           MOVE ")"      TO P3-R.
           MOVE ZAN-KAK2 TO P3-KAK2.
           MOVE "-"      TO P3-V.
           MOVE ZAN-HOK2 TO P3-HOK2.
           MOVE KNGNMN   TO P3-KNGNM.
           MOVE W-ZAN    TO P3-KIN.
           IF  W-SC       =  5
               MOVE 9          TO W-SC
           END-IF.
           IF  W-SC       =  0
               MOVE 5          TO W-SC
           END-IF.
       ST-60.
           PERFORM  PRI-RTN   THRU  PRI-EX.
           MOVE W-DCD    TO W-DC.
           IF  W-DC       =  2
               MOVE ZAN-DATA1  TO W-KEYDD
           END-IF.
       ST-70.
      *           READ ZAN-K NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" ZAN-K_PNAME1 BY REFERENCE ZAN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO END-99
           END-IF.
           MOVE ZERO     TO SET-WORK.
           IF  ZAN-KAK2   =  ZERO
               MOVE 1        TO W-DCD
               MOVE ZAN-DATA1 TO W-KEYD
               MOVE W-KAKD   TO AM-KEY
           ELSE
               MOVE 2        TO W-DCD
               MOVE ZAN-DATA2 TO W-KEYD
               MOVE W-KAKD    TO AM-KEY
           END-IF.
      *           READ AM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-MSG1" ERR-MSG1 "p"
                                        RETURNING RESU
               CALL "SD_Output" USING "ERR-KEY" ERR-KEY "p"
                                        RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                        RETURNING RESU
               CALL "SD_Output" USING "DISP-MSG-SPACE"
                DISP-MSG-SPACE "p"RETURNING RESU
               GO  TO  ST-70
           END-IF.
           IF  W-HOKD = ZERO
               PERFORM  MR1-SET-RTN  THRU  MR1-SET-EX
           ELSE
               PERFORM  MR2-SET-RTN  THRU  MR2-SET-EX
           END-IF.
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
           IF  W-ZAN  = ZERO
               GO  TO  ST-70
           END-IF.
           IF  ZAN-NO   NOT =  W-NO
               GO  TO  ST-90
           END-IF.
           IF  ZAN-KAK  NOT =  W-KAK
               GO  TO  ST-80
           END-IF.
           IF  W-DC         =  2
               IF  ZAN-DATA1      =  W-KEYDD
                   GO  TO  ST-50
               END-IF
           END-IF.
           IF  W-DC         =  2
               IF  ZAN-DATA1  NOT =  W-KEYDD
                   IF  W-SC           =  9
                       PERFORM SPRI-RTN    THRU SPRI-EX
                   END-IF
               END-IF
           END-IF.
           GO  TO  ST-40.
       ST-80.
           IF  W-DC         =  2
               IF  W-SC           =  9
                   PERFORM SPRI-RTN    THRU SPRI-EX
               END-IF
           END-IF.
           PERFORM APRI-RTN    THRU APRI-EX.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           MOVE       SPACE        TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           GO  TO  ST-30.
       ST-90.
           IF  W-DC         =  2
               IF  W-SC           =  9
                   PERFORM SPRI-RTN    THRU SPRI-EX
               END-IF
           END-IF.
           PERFORM APRI-RTN    THRU APRI-EX.
           PERFORM  HEAD-RTN  THRU  HEAD-EX.
           GO  TO  ST-20.
       CLSE-ENT.
       END-99.
           IF  W-ED    NOT  = 0
               IF  W-DC         =  2
                   IF  W-SC           =  9
                       PERFORM SPRI-RTN    THRU SPRI-EX
                   END-IF
               END-IF
           END-IF.
           IF  W-ED    NOT  = 0
               PERFORM APRI-RTN    THRU APRI-EX
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_Close".
           STOP       RUN.
       CLSE-EXT.
       HEAD-RTN.
           IF  W-POC        =      0
               MOVE     5            TO     W-POC
               GO   TO  HEAD-100
           END-IF.
           MOVE     SPACE        TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
       HEAD-100.
           ADD  1     TO  PCNT.
           MOVE  PCNT     TO  H-PAGE.
      *
           MOVE       SPACE        TO     F5-REC.
           MOVE       HEAD1        TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
           MOVE       HEAD2        TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
           MOVE       HEAD3        TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       HEAD-EX.
           EXIT.
       ZAN-SET-RTN.
           IF  TI > 12
               GO TO ZAN-SET-500
           END-IF.
           MOVE W-MZAN     TO W-ZENKI.
           MOVE FI          TO SOE.
       ZAN-SET-000.
           ADD W-MTJKR(SOE)     TO W-KARI.
           ADD W-MTJKS(SOE)     TO W-KASI.
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF.
           IF  SOE = 12
               MOVE 1     TO SOE
               GO TO ZAN-SET-000
           END-IF.
           ADD 1     TO SOE.
           GO TO ZAN-SET-000.
       ZAN-SET-500.
           IF  BS-PL = 0
               MOVE 1      TO SOE
           ELSE
               MOVE 13     TO SOE
           END-IF.
       ZAN-SET-600.
           ADD W-MTJKR(SOE)     TO W-KARI.
           ADD W-MTJKS(SOE)     TO W-KASI.
           IF  SOE = 12
               IF  DR-CR = 1
                   COMPUTE W-ZENKI = W-MZAN + W-KARI - W-KASI
               ELSE
                   COMPUTE W-ZENKI = W-MZAN + W-KASI - W-KARI
               END-IF
           END-IF.
           IF  SOE = 12
               MOVE W-ZENKI     TO W-MZAN
               MOVE ZERO        TO W-KARI W-KASI
           END-IF.
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF.
           IF  SOE = 15
               GO TO ZAN-SET-900
           END-IF.
           ADD 1     TO SOE.
           GO TO ZAN-SET-600.
       ZAN-SET-900.
           IF  DR-CR = 1
               COMPUTE W-ZAN = W-MZAN + W-KARI - W-KASI
           ELSE
               COMPUTE W-ZAN = W-MZAN - W-KARI + W-KASI
           END-IF.
       ZAN-SET-EX.
           EXIT.
       MR1-SET-RTN.
           MOVE W-KAKD   TO KZM-KEY.
      *           READ KZM-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KZM-F_PNAME1 BY REFERENCE KZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE KZM-R
           END-IF.
           MOVE ZERO           TO   W-MR  DCNT.
           MOVE KZM-ZAN        TO   W-MZAN.
       MR1-SET-000.
           ADD      1          TO   DCNT.
           IF  DCNT      >   15
               GO   TO   MR1-SET-EX
           END-IF.
           MOVE KZM-TJKR(DCNT) TO   W-MTJKR(DCNT).
           MOVE KZM-TJKS(DCNT) TO   W-MTJKS(DCNT).
           GO   TO   MR1-SET-000.
       MR1-SET-EX.
           EXIT.
       MR2-SET-RTN.
           MOVE W-KEYD   TO HZM-KEY.
      *           READ HZM-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HZM-F_PNAME1 BY REFERENCE HZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE HZM-R
           END-IF.
           MOVE ZERO           TO   W-MR  DCNT.
           MOVE HZM-ZAN        TO   W-MZAN.
       MR2-SET-000.
           ADD      1          TO   DCNT.
           IF  DCNT      >   15
               GO   TO   MR2-SET-EX
           END-IF.
           MOVE HZM-TJKR(DCNT) TO   W-MTJKR(DCNT).
           MOVE HZM-TJKS(DCNT) TO   W-MTJKS(DCNT).
           GO   TO   MR2-SET-000.
       MR2-SET-EX.
           EXIT.
       PRI-RTN.
           IF  W-POC    =  0
               MOVE       0            TO     CHK1
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           IF  CHK1     NOT  =  0
               GO    TO   PRI-10
           END-IF.
           IF  W-DCD    =  2
               IF  CHK2         =   0
                   CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
                   IF  LINAGECOUNTER  >  61
                       MOVE       0            TO     CHK
                       PERFORM  HEAD-RTN  THRU  HEAD-EX
                   END-IF
               END-IF
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  58
               MOVE       0            TO     CHK
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           MOVE       1            TO     CHK1.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P1         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       PRI-10.
           IF  W-DCD   =  2
               GO    TO   PRI-20
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               MOVE       0            TO     CHK
               GO   TO   PRI-RTN
           END-IF.
           MOVE       1            TO     CHK2.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P2         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
           ADD        W-ZAN        TO     W-AZAN.
           GO    TO   PRI-EX.
       PRI-20.
           IF  CHK2     =  1
               GO    TO   PRI-30
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  62
               MOVE       0            TO     CHK
               GO   TO   PRI-RTN
           END-IF.
           MOVE       1            TO     CHK2.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P2         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       PRI-30.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               MOVE       0            TO     CHK
               GO   TO   PRI-RTN
           END-IF.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P3         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
           ADD        W-ZAN        TO     W-SZAN    W-AZAN.
       PRI-EX.
           EXIT.
       SPRI-RTN.
           MOVE SPACE    TO W-P3.
           MOVE W-15K    TO P3-15K.
           MOVE W-20K    TO P3-20K.
           MOVE "　（　小計　）　　　"  TO P3-KNGNM.
           MOVE W-SZAN   TO P3-KIN.
       SPRI-10.
           IF  CHK1     NOT  =  0
               GO    TO   SPRI-20
           END-IF.
           IF  CHK2         =   0
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER  >  61
                   MOVE       0            TO     CHK
                   PERFORM  HEAD-RTN  THRU  HEAD-EX
               END-IF
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  62
               MOVE       0            TO     CHK
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           MOVE       1            TO     CHK1.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P1         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       SPRI-20.
           IF  CHK2     =  1
               GO    TO   SPRI-30
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  62
               MOVE       0            TO     CHK
               GO   TO   SPRI-10
           END-IF.
           MOVE       1            TO     CHK2.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P2         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       SPRI-30.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               MOVE       0            TO     CHK
               GO   TO   SPRI-10
           END-IF.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P3         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       SPRI-EX.
           EXIT.
       APRI-RTN.
           MOVE SPACE    TO W-P2.
           MOVE W-15K    TO P2-15K.
           MOVE W-20K    TO P2-20K.
           MOVE "　【　合　計　】　　"  TO P2-KNGNM.
           MOVE W-AZAN   TO P2-KIN.
       APRI-10.
           IF  CHK1     NOT  =  0
               GO    TO   APRI-20
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  62
               MOVE       0            TO     CHK
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           MOVE       1            TO     CHK1.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P1         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       APRI-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               MOVE       0            TO     CHK
               GO   TO   APRI-RTN
           END-IF.
           MOVE       1            TO     CHK2.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P2         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
       APRI-EX.
           EXIT.
       TEST-RTN.
           MOVE SPACE    TO W-P1.
           MOVE W-15K    TO P1-15K.
           MOVE W-20K    TO P1-20K.
           MOVE 9999     TO P1-KAK.
           MOVE ALL "Ｎ" TO P1-KNGNM.
           MOVE 9999999999 TO P1-KIN.
       TEST-10.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TPC "ACP-TPC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  TEST-10
           END-IF.
           IF  W-TPC NOT = 1 AND 9
               GO  TO  TEST-10
           END-IF.
           IF  W-TPC  = 1
               GO  TO  TEST-EX
           END-IF.
           IF  W-POC    =  0
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  63
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           MOVE       SPACE        TO     F5-REC.
           MOVE       W-P1         TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       SPACE        TO     F5-REC.
           GO    TO   TEST-10.
       TEST-EX.
           EXIT.
           COPY  LPMSG_PR.

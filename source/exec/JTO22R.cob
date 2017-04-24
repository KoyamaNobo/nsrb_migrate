       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JTO22R.
      ****************************************************************
      *    ÇnÅ^ÇkèÛãµñ‚çáÇπ                                          *
      *                                    89/ 8/10   NO.205 H.K     *
      ****************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
      *----< ‹∞∏ ¥ÿ± >----*
       77  END-SW        PIC 9(1).
       77  RED-SW        PIC 9(1).
       77  NN            PIC 9(2).
       77  NA            PIC 9(2).
       77  KAKU-W        PIC X(1).
       77  W-NOD         PIC 9(2).
       77  NO-SW         PIC 9(1).
       01  GAMEN-WK.
         02  GAMEN-TBL   OCCURS  21.
           03  W-NO      PIC ZZ.
           03  F         PIC X(1).
           03  W-TUKI    PIC Z9.
           03  W-POINT1  PIC X(1).
           03  W-NITI    PIC Z9.
           03  F         PIC X(1).
           03  W-KHH     PIC Z9.
           03  W-POINT2  PIC X(1).
           03  W-KMM     PIC Z9.
           03  F         PIC X(1).
           03  W-SHH     PIC Z9.
           03  W-POINT3  PIC X(1).
           03  W-SMM     PIC Z9.
           03  F         PIC X(1).
           03  W-SHORI   PIC N(2).
           03  F         PIC X(1).
           03  W-ZYOKYO  PIC N(2).
           03  F         PIC X(1).
           03  W-STS1    PIC X.
           03  F         PIC X(1).
           03  W-STS2    PIC 9(2).
           03  F         PIC X(1).
           03  W-KB      PIC N(2).
           03  F         PIC X(1).
           03  W-AITN    PIC N(2).
           03  F         PIC X(1).
           03  W-KENSU1  PIC Z,ZZ9.
           03  W-POINT4  PIC X(1).
           03  W-KENSU2  PIC Z,ZZ9.
           03  F         PIC X(1).
           03  W-START   PIC X(10).
           03  W-POINT6  PIC X(1).
           03  W-END     PIC X(10).
           03  F         PIC X(1).
       01  SUB-WK.
         02  SUB-W       PIC X(80).
       01  ERR-STAT      PIC X(02).
      *
           COPY     L-JOJF.
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-GAMEN.
         02  DSP-01      PIC X(80).
         02  DSP-02      PIC X(80).
         02  DSP-03      PIC X(80).
         02  DSP-04      PIC X(80).
         02  DSP-05      PIC X(80).
         02  DSP-06      PIC X(80).
         02  DSP-07      PIC X(80).
         02  DSP-08      PIC X(80).
         02  DSP-09      PIC X(80).
         02  DSP-10      PIC X(80).
         02  DSP-11      PIC X(80).
         02  DSP-12      PIC X(80).
         02  DSP-13      PIC X(80).
         02  DSP-14      PIC X(80).
         02  DSP-15      PIC X(80).
         02  DSP-16      PIC X(80).
         02  DSP-17      PIC X(80).
         02  DSP-18      PIC X(80).
         02  DSP-19      PIC X(80).
         02  DSP-20      PIC X(80).
      *                                               PREVIOUS  ATTRIBUTE.
       01  ACP-KAKU.
         02  INP-KAKU    PIC X .
       01  DSP-MSG.
         02  DSP-MSG1    PIC N(9)  VALUE
                         "ÇmÇdÇwÇsÉfÅ[É^óLÇË".
         02  DSP-MSG2    PIC N(9)  VALUE
                         "ÇmÇdÇwÇsÉfÅ[É^ñ≥Çµ".
         02  DSP-MSG3    PIC X(18) VALUE  " ".
      *==============================================================*
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "1600" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "X" "4" "1" "80" " " "DSP-GAMEN" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-01" BY REFERENCE GAMEN-TBL(1) "80" "1" "1" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" "X" "5" "1" "80" "DSP-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-02" BY REFERENCE GAMEN-TBL(1) "80" "1" "2" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" "X" "6" "1" "80" "DSP-02" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-03" BY REFERENCE GAMEN-TBL(1) "80" "1" "3" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" "X" "7" "1" "80" "DSP-03" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-04" BY REFERENCE GAMEN-TBL(1) "80" "1" "4" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-05" "X" "8" "1" "80" "DSP-04" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-05" BY REFERENCE GAMEN-TBL(1) "80" "1" "5" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06" "X" "9" "1" "80" "DSP-05" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-06" BY REFERENCE GAMEN-TBL(1) "80" "1" "6" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" "X" "10" "1" "80" "DSP-06" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-07" BY REFERENCE GAMEN-TBL(1) "80" "1" "7" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-08" "X" "11" "1" "80" "DSP-07" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-08" BY REFERENCE GAMEN-TBL(1) "80" "1" "8" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-09" "X" "12" "1" "80" "DSP-08" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-09" BY REFERENCE GAMEN-TBL(1) "80" "1" "9" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-10" "X" "13" "1" "80" "DSP-09" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-10" BY REFERENCE GAMEN-TBL(1) "80" "1" "10" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" "X" "14" "1" "80" "DSP-10" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-11" BY REFERENCE GAMEN-TBL(1) "80" "1" "11" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-12" "X" "15" "1" "80" "DSP-11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-12" BY REFERENCE GAMEN-TBL(1) "80" "1" "12" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-13" "X" "16" "1" "80" "DSP-12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-13" BY REFERENCE GAMEN-TBL(1) "80" "1" "13" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-14" "X" "17" "1" "80" "DSP-13" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-14" BY REFERENCE GAMEN-TBL(1) "80" "1" "14" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-15" "X" "18" "1" "80" "DSP-14" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-15" BY REFERENCE GAMEN-TBL(1) "80" "1" "15" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-16" "X" "19" "1" "80" "DSP-15" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-16" BY REFERENCE GAMEN-TBL(1) "80" "1" "16" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-17" "X" "20" "1" "80" "DSP-16" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-17" BY REFERENCE GAMEN-TBL(1) "80" "1" "17" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-18" "X" "21" "1" "80" "DSP-17" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-18" BY REFERENCE GAMEN-TBL(1) "80" "1" "18" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-19" "X" "22" "1" "80" "DSP-18" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-19" BY REFERENCE GAMEN-TBL(1) "80" "1" "19" 80
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-20" "X" "23" "1" "80" "DSP-19" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-20" BY REFERENCE GAMEN-TBL(1) "80" "1" "20" 80
            RETURNING RESU.
      *ACP-KAKU
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INP-KAKU" "X" "24" "74" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "INP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-MSG
       CALL "SD_Init" USING 
            "DSP-MSG" " " "24" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG1" "N" "24" "2" "18" " " "DSP-MSG" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG2" "N" "24" "2" "18" "DSP-MSG1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG3" "X" "24" "2" "18" "DSP-MSG2" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *--------------------------------------------------------------*
      *    “ ≤ ›  Ÿ ∞ ¡ ›                                            *
      *--------------------------------------------------------------*
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
               UNTIL  END-SW  =  9.
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
       MAINLINE-END.
           CALL "DB_Close".
           STOP RUN.
      *--------------------------------------------------------------*
      *    P R O C  -  R T N                                         *
      *--------------------------------------------------------------*
       PROC-RTN.
           PERFORM  DETL-RTN  THRU  DETL-RTN-EXIT
               UNTIL  NA      =  21.
           PERFORM  DISP-RTN  THRU  DISP-RTN-EXIT.
           PERFORM  MESG-RTN  THRU  MESG-RTN-EXIT.
       PROC-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    D E T L  -  R T N                                         *
      *--------------------------------------------------------------*
       DETL-RTN.
           PERFORM  READ-RTN  THRU  READ-RTN-EXIT.
           PERFORM  MOVE-RTN  THRU  MOVE-RTN-EXIT.
       DETL-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    I N I T  -  R T N                                         *
      *--------------------------------------------------------------*
       INIT-RTN.
           MOVE     0  TO  END-SW  RED-SW  NN  NA  W-NOD.
           MOVE  SPACE TO  GAMEN-WK.
           CALL "SD_Screen_Output" USING "SJTO22" RETURNING RESU.
       INIT-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    R E A D  -  R T N                                         *
      *--------------------------------------------------------------*
       READ-RTN.
           IF  NN       =  0
               GO TO READ-010
           END-IF
           IF  NN  NOT  =  10
               GO TO READ-RTN-EXIT
           END-IF.
       READ-010.
      *           READ   JOJF  NEXT  UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JOJF_PNAME1 BY REFERENCE JOJF-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               MOVE  21 TO  NA
               GO TO READ-RTN-EXIT
           END-IF
           MOVE  0       TO  NN  NO-SW.
           MOVE  1       TO  RED-SW.
       READ-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    M O V E  -  R T N                                         *
      *--------------------------------------------------------------*
       MOVE-RTN.
           IF  END-SW   =  9
               GO TO MOVE-RTN-EXIT
           END-IF.
       MOVE-010.
           IF  NN          =  10
               GO TO MOVE-RTN-EXIT
           END-IF
           ADD 1       TO  NN.
           IF  JOJF-08(NN) =  0
               GO TO MOVE-010
           END-IF
           ADD 1       TO  NA.
           MOVE 0        TO W-NO(NA).
           IF  NO-SW      =  0
               IF  JOJF-061   =  1
                   MOVE   1        TO  NO-SW
                   ADD    1        TO  W-NOD
                   MOVE   W-NOD    TO  W-NO(NA)
               END-IF
           END-IF
           IF  JOJF-08(NN) =  01
               MOVE  "ÇbÇe"  TO  W-KB(NA)
           ELSE
               IF  JOJF-08(NN)  =  02
                   MOVE  "íºëó"  TO  W-KB(NA)
               ELSE
                   IF  JOJF-08(NN)  =  03
                       MOVE  "ïiñº"  TO  W-KB(NA)
                   ELSE
                       IF  JOJF-08(NN)  =  04
                           MOVE  "ìXñº"  TO  W-KB(NA)
                       ELSE
                           IF  JOJF-08(NN)  =  11
                               MOVE  "èoâ◊"  TO  W-KB(NA)
                           ELSE
                               IF  JOJF-08(NN)  =  12
                                   MOVE  "â◊éD"  TO  W-KB(NA)
                               ELSE
                                   IF  JOJF-08(NN)  =  13
                                       MOVE  "ëóèÛ"  TO  W-KB(NA)
                                   ELSE
                                       IF  JOJF-08(NN)  =  14
                                           MOVE  "ÉèÅ["  TO  W-KB(NA)
                                       ELSE
                                           IF  JOJF-08(NN)  =  15
                                           MOVE  "ÉiÉt"  TO  W-KB(NA)
                                           ELSE
                                           IF  JOJF-08(NN)  =  16
                                           MOVE  "ÉgÉâ"  TO  W-KB(NA)
                                           ELSE
                                           IF  JOJF-08(NN)  =  17
                                           MOVE  "ê‘Çø"  TO  W-KB(NA)
                                           ELSE
                                           MOVE  JOJF-08(NN) TO W-KB(NA)
                                           END-IF
                                           END-IF
                                           END-IF
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE  "Å@Å@"   TO W-AITN(NA).
           IF  JOJF-07      =   001
               MOVE  "ã ìá"   TO W-AITN(NA)
           END-IF
           IF  JOJF-07      =   002
               MOVE  "í√éR"   TO W-AITN(NA)
           END-IF
           IF  JOJF-07      =   003
               MOVE  "óºîı"   TO W-AITN(NA)
           END-IF
           MOVE  JOJF-09(NN)  TO  W-KENSU1(NA).
           IF  JOJF-061 NOT = 0
               MOVE  JOJF-10(NN)  TO  W-KENSU2(NA)
               MOVE  "("        TO  W-POINT4(NA)
           END-IF
           MOVE  JOJF-11(NN)  TO  W-START(NA).
           MOVE  JOJF-12(NN)  TO  W-END(NA).
           MOVE  "-"          TO  W-POINT6(NA).
           IF  RED-SW     =  1
               GO TO MOVE-015
           END-IF
           IF  NA   NOT   =  21
               GO TO MOVE-030
           END-IF.
       MOVE-015.
           MOVE  JOJF-021     TO  W-TUKI(NA).
           MOVE  JOJF-022     TO  W-NITI(NA).
           MOVE  "/"          TO  W-POINT1(NA).
           MOVE  JOJF-031     TO  W-KHH(NA).
           MOVE  JOJF-032     TO  W-KMM(NA).
           MOVE  JOJF-041     TO  W-SHH(NA).
           MOVE  JOJF-042     TO  W-SMM(NA).
           MOVE  ":"          TO  W-POINT2(NA)  W-POINT3(NA).
           IF  JOJF-05    =  1
               MOVE  "ëóêM"   TO  W-SHORI(NA)
           ELSE
               IF  JOJF-05 = 2
                   MOVE  "éÛêM"   TO  W-SHORI(NA)
               ELSE
                   IF  JOJF-05 = 3
                       MOVE  "ëóïœ"   TO  W-SHORI(NA)
                   ELSE
                       MOVE  SPACE      TO  W-SHORI(NA)
                   END-IF
               END-IF
           END-IF
           IF  JOJF-061   =  0
               MOVE  "ñ¢Å@"   TO  W-ZYOKYO(NA)
               GO TO MOVE-020
           END-IF
           IF  JOJF-061   =  1
               MOVE  "ê≥èÌ"   TO  W-ZYOKYO(NA)
               GO TO MOVE-020
           END-IF
           IF  JOJF-061   =  7
               MOVE  "íÜíf"   TO  W-ZYOKYO(NA)
               GO TO MOVE-020
           END-IF
           IF  JOJF-061   =  8
               MOVE  "àŸèÌ"   TO  W-ZYOKYO(NA)
               GO TO MOVE-020
           END-IF
           IF  JOJF-061   =  9
               IF  JOJF-01   =   1
                   MOVE  "çÏíÜ"   TO  W-ZYOKYO(NA)
                   GO TO MOVE-020
               ELSE
                   MOVE  "ïsâ¬"   TO  W-ZYOKYO(NA)
                   GO TO MOVE-020
               END-IF
           END-IF
           IF  JOJF-061   =  6
               IF  JOJF-05 = 1
                   MOVE  "ëóíÜ"   TO  W-ZYOKYO(NA)
                   GO TO MOVE-020
               ELSE
                   IF JOJF-05 = 2
                       MOVE  "éÛíÜ" TO W-ZYOKYO(NA)
                       GO TO MOVE-020
                   END-IF
               END-IF
           END-IF
           MOVE  SPACE    TO  W-ZYOKYO(NA).
       MOVE-020.
           MOVE  ZERO     TO  RED-SW.
           IF  JOJF-061   =  8
               MOVE  JOJF-062  TO  W-STS1(NA)
               MOVE  JOJF-063  TO  W-STS2(NA)
           END-IF.
       MOVE-030.
           IF  NA         =  21
               GO TO MOVE-RTN-EXIT
           END-IF
           IF  NN   NOT   =  10
               GO TO MOVE-010
           END-IF.
       MOVE-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    M E S G  -  R T N                                         *
      *--------------------------------------------------------------*
       MESG-RTN.
           IF  END-SW      =  9
               CALL "SD_Output" USING
                "DSP-MSG2" DSP-MSG2 "p" RETURNING RESU
               GO TO MESG-010
           END-IF
           MOVE  SUB-WK   TO  GAMEN-TBL(1).
           CALL "SD_Output" USING
            "DSP-MSG1" DSP-MSG1 "p" RETURNING RESU.
       MESG-010.
           CALL "SD_Accept" USING BY REFERENCE INP-KAKU "INP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MSG3" DSP-MSG3 "p" RETURNING RESU.
           IF  END-STS       = "P9"
               MOVE  9  TO  END-SW
               GO TO MESG-RTN-EXIT
           END-IF
           IF  END-SW  NOT   =  9
               IF  END-STS  NOT  =  "01"
                   GO TO MESG-010
               ELSE
                   GO TO MESG-RTN-EXIT
               END-IF
           END-IF.
       MESG-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    D I S P  -  R T N                                         *
      *--------------------------------------------------------------*
       DISP-RTN.
           CALL "SD_Output" USING
            "DSP-GAMEN" DSP-GAMEN "p" RETURNING RESU.
           IF  GAMEN-TBL(21)  NOT  =  SPACE
               MOVE  GAMEN-TBL(21)  TO  SUB-WK
           END-IF
           MOVE   SPACE      TO   GAMEN-WK.
           MOVE   1          TO   NA.
       DISP-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    O P E N  -  R T N                                         *
      *--------------------------------------------------------------*
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
       OPEN-RTN-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *    C L S E  -  R T N                                         *
      *--------------------------------------------------------------*
       CLSE-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
       CLSE-RTN-EXIT.
           EXIT.

       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT630R.
       AUTHOR.                        MAYUMI.I.
      ***************************************************
      *    PROGRAM        : éÛíçáÇñ‚çáÇπÅ@Å@Å@Å@Å@Å@Å@  *
      *    DATA WRITTEN   : 91/09/18                    *
      *    SCREEN USED    : SJ631R                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : UNUSED                      *
      *    COMPILE TYPE   : COBOL85 (74MODE)            *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       01  W-AREA.
           02  W-01                  PIC 9(04).
           02  W-02                  PIC 9(06).
           02  S-01                  PIC 9(04).
           02  S-02                  PIC 9(06).
           02  WW-01.
               03  WW-011            PIC 9(04).
               03  WW-012            PIC 9(03).
           02  OKC                   PIC 9(01).
           02  LIN                   PIC 9(02).
           02  SW                    PIC X(03).
      *    SW  =  " ON" ---> É}ÉXÉ^ÇÇqÇdÇ`ÇcÇµÇƒÅCÇhÇmÇuÇ`ÇkÇhÇcÇÃéûÅB
      *    SW  =  "OFF" ---> É}ÉXÉ^ÇÇqÇdÇ`ÇcÇµÇƒÅCÇmÇnÇqÇlÇ`ÇkÇÃéûÅB
           02  W-AREA1.
               03  W-11                  PIC 9(04).
               03  W-13.
                   04  W-131             PIC N(22).
                   04  W-132             PIC N(02).
               03  W-14                  PIC 9(06).
               03  W-15                  PIC 9(01).
               03  W-16                  PIC N(01).
               03  W-171                 PIC 9(02).
               03  W-172                 PIC 9(02).
               03  W-173                 PIC 9(02).
               03  W-18                  PIC S9(06).
       01  HINMEI                    PIC N(24)   VALUE
           "ïiñºÅHÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".
       01  CHOKUSOU                  PIC N(24)   VALUE
           "íºëóêÊÅHÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".
      *
       COPY    LWMSG.
      *
           COPY  LJMSTD.
           COPY  LIHIM2.
           COPY  LITCM.
           COPY  LWTNAF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLR-01.
           02  FILLER   PIC  X(12) VALUE  "CLEAR SCREEN".
       01  CLR-AREA.
           02  CLR-02.
               03  FILLER   PIC X(04)   VALUE " ".
               03  FILLER   PIC X(48)   VALUE " ".
               03  FILLER   PIC X(06)   VALUE " ".
               03  FILLER   PIC X(48)   VALUE " ".
           02  CLR-03.
               03  FILLER   PIC  X(11) VALUE  "CLEAR  DATA".
       01  CLR-AREA1.
           02  GAMEN-CLR.
               03  FILLER   PIC  X(11) VALUE  "CLEAR  DATA".
       01  DSP-AREA.
           02  DSP-01     PIC N(24).
           02  DSP-02     PIC N(24).
           02  DSP-11     PIC 9(04).
           02  DSP-13     PIC N(22).
           02  DSP-18     PIC ----,--9 .
           02  DSP-14     PIC 9(06).
           02  DSP-15     PIC 9(01).
           02  DSP-16     PIC N(01).
           02  DSP-17.
               03  DSP-171    PIC Z9 .
               03  DSP-172    PIC Z9 .
               03  DSP-173    PIC Z9 .
           02  DSP-A      PIC X(01)
                          VALUE  "-".
           02  DSP-B      PIC X(01)
                          VALUE  "/".
           02  DSP-C      PIC X(01)
                          VALUE  "/".
           02  DSP-SPACE  PIC  X(11) VALUE  "CLEAR  DATA".
       01  ACP-AREA.
           02  ACP-01  PIC 9(04).
           02  ACP-02  PIC 9(06).
           02  ACP-OKC PIC 9(01).
       01  DSP-ERR.
           02  MSG-01          PIC  X(030) VALUE
             "íºëóêÊÉ}ÉXÉ^Åiìæà”êÊÅjÅ@ñ¢ìoò^".
           02  MSG-02          PIC  X(022) VALUE
             "èoâ◊ïiñºÉ}ÉXÉ^Å@ñ¢ìoò^".
           02  MSG-03          PIC  X(014) VALUE
             "éüÉfÅ[É^Å@ñ≥Çµ".
      *
       01  DISP-MSG-SPACE1.
           02  FILLER   PIC X(40)     VALUE " ".
      *
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLR-01
       CALL "SD_Init" USING
           "CLR-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-01" "X" "1" "0" "12" " " "CLR-01" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "117" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-02" " " "0" "0" "106" " " "CLR-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-02" "X" "1" "23" "4" " " "CLR-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-02" "X" "1" "30" "48" "01CLR-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-02" "X" "2" "23" "6" "02CLR-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-02" "X" "2" "30" "48" "03CLR-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-03" " " "0" "0" "11" "CLR-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-03" "X" "4" "23" "11" " " "CLR-03"  RETURNING RESU.
      *CLR-AREA1
       CALL "SD_Init" USING 
            "CLR-AREA1" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "GAMEN-CLR" " " "0" "0" "11" " " "CLR-AREA1" RETURNING RESU.
       CALL "SD_Init" USING 
            "01GAMEN-CLR" "X" "LIN" "23" "11" " " "GAMEN-CLR"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "181" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "N" "1" "30" "48" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-01" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" "N" "2" "30" "48" "DSP-01" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-02" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" "9" "LIN" "2" "4" "DSP-02" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-11" BY REFERENCE W-11 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-13" "N" "LIN" "6" "44" "DSP-11" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-13" BY REFERENCE W-131 "44" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-18" "----,--9" "LIN" "51" "8" "DSP-13" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-18" BY REFERENCE W-18 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-14" "9" "LIN" "60" "6" "DSP-18" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-14" BY REFERENCE W-14 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-15" "9" "LIN" "67" "1" "DSP-14" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-15" BY REFERENCE W-15 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-16" "N" "LIN" "69" "2" "DSP-15" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-16" BY REFERENCE W-16 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-17" " " "0" "0" "6" "DSP-16" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-171" "Z9" "LIN" "72" "2" " " "DSP-17" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-171" BY REFERENCE W-171 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-172" "Z9" "LIN" "75" "2" "DSP-171" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-172" BY REFERENCE W-172 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-173" "Z9" "LIN" "78" "2" "DSP-172" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-173" BY REFERENCE W-173 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-A" "X" "LIN" "66" "1" "DSP-17" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-B" "X" "LIN" "74" "1" "DSP-A" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-C" "X" "LIN" "77" "1" "DSP-B" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SPACE" "X" "LIN" "0" "11" "DSP-C" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-01" "9" "1" "23" "4" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-01" BY REFERENCE W-01 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-02" "9" "2" "23" "6" "ACP-01" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-02" BY REFERENCE W-02 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "62" "1" "ACP-02" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "66" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "MSG-01" "X" "24" "1" "30" " " "DSP-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "MSG-02" "X" "24" "1" "22" "MSG-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "MSG-03" "X" "24" "1" "14" "MSG-02" " "  RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE1" " " "24" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "COLUMN" "X" "24" "1" "40" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       HAJIME.
           PERFORM   INI-RTN    THRU  INI-EX.
           PERFORM   MAIN-RTN   THRU  MAIN-EX.
           PERFORM   END-RTN    THRU  END-EX.
           CALL "DB_Close".
           STOP  RUN.
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       MAIN-RTN.
           MOVE  W-01   TO  S-01.
       MAIN-000.
           CALL "SD_Accept" USING BY REFERENCE ACP-01 "ACP-01" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MAIN-EX
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-RTN
           END-IF
           CALL "SD_Output" USING "ACP-01" ACP-01 "p" RETURNING RESU.
       MAIN-001.
           MOVE  W-01     TO  TC-TCD.
           MOVE  "001"    TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           IF  SW  =  " ON"
               CALL "SD_Output" USING
                "MSG-01" MSG-01 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "DSP-01" DSP-01 "p" RETURNING RESU.
      *
       MAIN-010.
           MOVE  W-02   TO  S-02.
       MAIN-011.
           CALL "SD_Accept" USING BY REFERENCE ACP-02 "ACP-02" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MAIN-RTN
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-010
           END-IF
           CALL "SD_Output" USING "ACP-02" ACP-02 "p" RETURNING RESU.
           IF  W-02  =  ZERO
               MOVE  SPACE     TO  HI-NAME
               GO  TO  MAIN-020
           END-IF.
       MAIN-012.
           MOVE  W-02     TO  HI-MHCD HI-KEY.
           PERFORM  HIM-READ-RTN     THRU  HIM-READ-EX.
           IF  SW  =  " ON"
               CALL "SD_Output" USING
                "MSG-02" MSG-02 "p" RETURNING RESU
           END-IF.
       MAIN-020.
           CALL "SD_Output" USING "DSP-02" DSP-02 "p" RETURNING RESU.
      *
           MOVE  3        TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
      *
           INITIALIZE  JMSTD-KEY2.
           MOVE  W-01     TO  JMSTD-04.
           MOVE  W-02     TO  JMSTD-05.
      *           START  JMSTD  KEY  NOT  <  JMSTD-KEY2  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY2" " NOT < " JMSTD-KEY2
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               GO  TO  MAIN-010
           END-IF.
       MAIN-030.
      ***  éÛíçÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  JMSTD  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-040
           END-IF
           IF  W-01  NOT =  JMSTD-04
               GO  TO  MAIN-040
           END-IF
           IF  W-02  NOT =  ZERO
               IF  W-02  NOT =  JMSTD-05
                   GO  TO  MAIN-040
               END-IF
           END-IF.
       NEXT-DSP.
           ADD  1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           IF  LIN  =  24
               GO  TO  MAIN-OKC
           END-IF
           PERFORM  DSP-RTN     THRU  DSP-EX.
           GO  TO  MAIN-030.
       MAIN-040.
           IF  LIN  =  3
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               GO  TO  MAIN-010
           END-IF
           IF  LIN  NOT =  23
               ADD  1     TO  LIN
               CALL "SD_Arg_Match_Line" USING
                "LIN" "2" LIN RETURNING RESU
               CALL "SD_Output" USING
                "GAMEN-CLR" GAMEN-CLR "p" RETURNING RESU
           END-IF.
       MAIN-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MAIN-010
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-OKC
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  OKC  NOT  =  "1" AND "9"
               GO  TO  MAIN-OKC
           END-IF
           IF  OKC  =  "9"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               PERFORM  CLR-RTN     THRU  CLR-EX
               GO  TO  MAIN-RTN
           END-IF
           IF  LIN  =  24
               MOVE  3        TO  LIN
               CALL "SD_Arg_Match_Line" USING
                "LIN" "2" LIN RETURNING RESU
               GO  TO  NEXT-DSP
           ELSE
               CALL "SD_Output" USING "MSG-03" MSG-03 "p" RETURNING RESU
               GO  TO  MAIN-OKC
           END-IF.
       MAIN-EX.
           EXIT.
      ************************************
      *    ÇcÇrÇoÅ|ÇqÇsÇmÅ@Å@Å@Å@        *
      ************************************
       DSP-RTN.
           MOVE  JMSTD-10         TO  W-11.
           MOVE  JMSTD-04         TO  TC-TCD.
           MOVE  JMSTD-10         TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           IF  SW  =  " ON"
               MOVE  CHOKUSOU     TO  W-13
           ELSE
               MOVE  TC-NAME      TO  W-13
           END-IF
           IF  JMSTD-04      NOT =  9850
               GO  TO  DSP-10
           END-IF
           IF  JMSTD-23          =  ZERO
               GO  TO  DSP-10
           END-IF
           MOVE  JMSTD-23         TO  W-11.
           MOVE  JMSTD-23         TO  WTNA-KEY.
           PERFORM  WTN-READ-RTN     THRU  WTN-READ-EX.
           IF  SW  =  " ON"
               MOVE  CHOKUSOU     TO  W-13
           ELSE
               MOVE  WTNA-NAME    TO  W-13
           END-IF.
       DSP-10.
           CALL "SD_Output" USING "DSP-11" DSP-11 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-13" DSP-13 "p" RETURNING RESU.
           COMPUTE  W-18      =  JMSTD-1111(01)  +  JMSTD-1111(02)  +
                                 JMSTD-1111(03)  +  JMSTD-1111(04)  +
                                 JMSTD-1111(05)  +  JMSTD-1111(06)  +
                                 JMSTD-1111(07)  +  JMSTD-1111(08)  +
                                 JMSTD-1111(09)  +  JMSTD-1111(10)  -
                                 JMSTD-1211(01)  -  JMSTD-1211(02)  -
                                 JMSTD-1211(03)  -  JMSTD-1211(04)  -
                                 JMSTD-1211(05)  -  JMSTD-1211(06)  -
                                 JMSTD-1211(07)  -  JMSTD-1211(08)  -
                                 JMSTD-1211(09)  -  JMSTD-1211(10).
           COMPUTE  W-18      =  W-18            -
                                 JMSTD-141 (01)  -  JMSTD-141 (02)  -
                                 JMSTD-141 (03)  -  JMSTD-141 (04)  -
                                 JMSTD-141 (05)  -  JMSTD-141 (06)  -
                                 JMSTD-141 (07)  -  JMSTD-141 (08)  -
                                 JMSTD-141 (09)  -  JMSTD-141 (10)  -
                                 JMSTD-151 (01)  -  JMSTD-151 (02)  -
                                 JMSTD-151 (03)  -  JMSTD-151 (04)  -
                                 JMSTD-151 (05)  -  JMSTD-151 (06)  -
                                 JMSTD-151 (07)  -  JMSTD-151 (08)  -
                                 JMSTD-151 (09)  -  JMSTD-151 (10).
           CALL "SD_Output" USING "DSP-18" DSP-18 "p" RETURNING RESU.
           MOVE  JMSTD-07     TO  W-14.
           MOVE  JMSTD-08     TO  W-15.
           IF  JMSTD-01  =  ZERO
               MOVE  "Å@"         TO  W-16
           END-IF
           IF  JMSTD-01  =  5
               MOVE  "óa"         TO  W-16
           END-IF
           IF  JMSTD-01  =  6
               MOVE  "éÊ"         TO  W-16
           END-IF
           MOVE  JMSTD-0212   TO  W-171.
           MOVE  JMSTD-022    TO  W-172.
           MOVE  JMSTD-023    TO  W-173.
           CALL "SD_Output" USING "DSP-14" DSP-14 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-A" DSP-A "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-15" DSP-15 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-16" DSP-16 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-17" DSP-17 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-B" DSP-B "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-C" DSP-C "p" RETURNING RESU.
       DSP-EX.
           EXIT.
      *********************************************
      *    ÇbÇkÇqÅ|ÇqÇsÇm                         *
      *********************************************
       CLR-RTN.
           CALL "SD_Output" USING
            "CLR-AREA" CLR-AREA "p" RETURNING RESU.
           INITIALIZE  W-AREA.
       CLR-EX.
           EXIT.
      *********************************************
      *    ÇhÇmÇhÅ|ÇqÇsÇm                         *
      *********************************************
       INI-RTN.
           CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ631R" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
       INI-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      ************************************
      *    íºëóêÊÉ}ÉXÉ^Å@ÇqÇdÇ`Çc        *
      ************************************
       TCM-READ-RTN.
           MOVE  "OFF"     TO    SW.
      ***  íºëóêÊÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  TC-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  " ON"     TO    SW
               MOVE  SPACE     TO   TC-NAME
           END-IF.
       TCM-READ-EX.
           EXIT.
       WTN-READ-RTN.
           MOVE  "OFF"     TO    SW.
      *           READ  WTNAF UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  " ON"     TO    SW
               MOVE  SPACE     TO   WTNA-NAME
           END-IF.
       WTN-READ-EX.
           EXIT.
      ************************************
      *    ïiñºÉ}ÉXÉ^Å@ÇqÇdÇ`Çc      *
      ************************************
       HIM-READ-RTN.
           MOVE  "OFF"     TO    SW.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  " ON"     TO    SW
               MOVE  SPACE     TO   HI-NAME
           END-IF.
       HIM-READ-EX.
           EXIT.
      *
       COPY LPMSG.
      *

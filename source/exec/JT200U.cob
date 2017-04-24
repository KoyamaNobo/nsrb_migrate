       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT200U.
      ******************************************************************
      *    <<REMARKS>>                                                 *
      *    FUNCTION.......  ì˙éüÅ@åJâz                                 *
      *    AUTHOR.........  Y.KATOH                                    *
      *    COMPILE MODE...  NORMAL                                     *
      *    SCREEN.........  XXXXX                                      *
      *    RELEASE DATE...  62/08/17         (REV.001)                 *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        NEAC-SYSTEM100.
       OBJECT-COMPUTER.        NEAC-SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT             PIC  X(02).
       77  W-FILE               PIC  X(13).
       77  ERR-SW               PIC  9(01)  VALUE  0.
       77  WJSTR-KEY            PIC  X(07).
       77  WJSTR-01             PIC  X(06).
       77  SEN-SW               PIC  9(01).
       01  W-DATA.
           02  W-DATE.
             03  W-NEN          PIC  9(04).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1       PIC  9(02).
               04  W-NEN2       PIC  9(02).
             03  W-GP           PIC  9(04).
           02  W-DATEL REDEFINES W-DATE.
             03  F              PIC  9(02).
             03  W-DATES        PIC  9(06).
           02  W-NGP            PIC  9(08).
      *
       COPY  LWMSG.
      *
           COPY  LIBFDD.
           COPY  L-JSTR.
           COPY  L-JNIF.
           COPY  LOKJF.
           COPY  LITDNW.
           COPY  LITDNN.
           COPY  L-TDIF.
           COPY  LITDNA.
           COPY  LITCM.
           COPY  LTDNKN.
           COPY  LJSTRR.
      *****   ÉiÉtÉRêîó ÉtÉ@ÉCÉã 45/5   ********************************
      *FD  NSURYOF
       01  NSURYOF_JT200U.
           02  NSURYOF_PNAME1 PIC  X(007) VALUE "NSURYOF".
           02  F              PIC  X(001).
           02  NSURYOF_LNAME  PIC  X(014) VALUE "NSURYOF_JT200U".
           02  F              PIC  X(001).
           02  NSURYOF_KEY1   PIC  X(100) VALUE SPACE.
           02  NSURYOF_SORT   PIC  X(100) VALUE SPACE.
           02  NSURYOF_IDLST  PIC  X(100) VALUE SPACE.
           02  NSURYOF_RES    USAGE  POINTER.
       01  NSURYO-R.
           02  NSURYO-01      PIC 9(06).
           02  NSURYO-02      PIC 9(06).
           02  NSURYO-03      PIC 9(03).
           02  NSURYO-04      PIC 9(08).
           02  NSURYO-05      PIC 9(06).
           02  NSURYO-06      PIC 9(08).
           02  NSURYO-07      PIC 9(04).
           02  NSURYO-08      PIC 9(02).
           02  NSURYO-09      PIC 9(02).
       77  F                  PIC X(01).
      *****   ÉiÉtÉRî†êîÉtÉ@ÉCÉã 36/7   ********************************
      *FD  NHAKOF
       01  NHAKOF_JT200U.
           02  NHAKOF_PNAME1  PIC  X(006) VALUE "NHAKOF".
           02  F              PIC  X(001).
           02  NHAKOF_LNAME   PIC  X(013) VALUE "NHAKOF_JT200U".
           02  F              PIC  X(001).
           02  NHAKOF_KEY1    PIC  X(100) VALUE SPACE.
           02  NHAKOF_SORT    PIC  X(100) VALUE SPACE.
           02  NHAKOF_IDLST   PIC  X(100) VALUE SPACE.
           02  NHAKOF_RES     USAGE  POINTER.
       01  NHAKO-R.
           02  NHAKO-01       PIC 9(06).
           02  NHAKO-02       PIC 9(06).
           02  NHAKO-03       PIC 9(03).
           02  NHAKO-04       PIC 9(01).
           02  NHAKO-05       PIC 9(06).
           02  NHAKO-06       PIC 9(08).
           02  NHAKO-07       PIC 9(04).
           02  NHAKO-08       PIC 9(02).
       77  F                  PIC X(01).
      *****   ÉiÉtÉRêîó í˘ê≥ÉtÉ@ÉCÉã 102/5  ****************************
      *FD  TEISEIF
       01  TEISEIF_JT200U.
           02  TEISEIF_PNAME1 PIC  X(008) VALUE "NTEISEIF".
           02  F              PIC  X(001).
           02  TEISEIF_LNAME  PIC  X(014) VALUE "TEISEIF_JT200U".
           02  F              PIC  X(001).
           02  TEISEIF_KEY1   PIC  X(100) VALUE SPACE.
           02  TEISEIF_SORT   PIC  X(100) VALUE SPACE.
           02  TEISEIF_IDLST  PIC  X(100) VALUE SPACE.
           02  TEISEIF_RES    USAGE  POINTER.
       01  TEISEI-R.
           02  TEISEI-01      PIC 9(06).
           02  TEISEI-02      PIC 9(06).
           02  TEISEI-03      PIC 9(03).
           02  TEISEI-04      PIC 9(08).
           02  TEISEI-05      PIC 9(05).
           02  TEISEI-051     PIC 9(01).
           02  TEISEI-06      PIC 9(08).
           02  TEISEI-07      PIC 9(04).
           02  TEISEI-08      PIC 9(02).
           02  TEISEI-09      PIC 9(02).
           02  TEISEI-21      PIC X(25).
           02  TEISEI-22      PIC X(24).
           02  TEISEI-23      PIC X(08).
       77  F                  PIC X(01).
      *FD  OKJRF
       01  OKJRF_JT200U.
           02  OKJRF_PNAME1          PIC  X(005) VALUE "OKJRF".
           02  F                     PIC  X(001).
           02  OKJRF_LNAME           PIC  X(012) VALUE "OKJRF_JT200U".
           02  F                     PIC  X(001).
           02  OKJRF_KEY1            PIC  X(100) VALUE SPACE.
           02  OKJRF_SORT            PIC  X(100) VALUE SPACE.
           02  OKJRF_IDLST           PIC  X(100) VALUE SPACE.
           02  OKJRF_RES             USAGE  POINTER.
       01  OKJR-R.
           02  OKJR-D                PIC X(56).
           02  OKJR-DATE             PIC 9(08).
       77  F                         PIC X(01).
      *FD  JNIRF
       01  JNIRF_JT200U.
           02  JNIRF_PNAME1          PIC  X(005) VALUE "JNIRF".
           02  F                     PIC  X(001).
           02  JNIRF_LNAME           PIC  X(012) VALUE "JNIRF_JT200U".
           02  F                     PIC  X(001).
           02  JNIRF_KEY1            PIC  X(100) VALUE SPACE.
           02  JNIRF_SORT            PIC  X(100) VALUE SPACE.
           02  JNIRF_IDLST           PIC  X(100) VALUE SPACE.
           02  JNIRF_RES             USAGE  POINTER.
       01  JNIR-R.
           02  JNIR-D                PIC X(128).
           02  F                     PIC X(34).
           02  JNIR-DATE             PIC 9(08).
       77  F                         PIC X(01).
      *FD  TDNWRF
       01  TDNWRF_JT200U.
           02  TDNWRF_PNAME1         PIC  X(006) VALUE "TDNWRF".
           02  F                     PIC  X(001).
           02  TDNWRF_LNAME          PIC  X(013) VALUE "TDNWRF_JT200U".
           02  F                     PIC  X(001).
           02  TDNWRF_KEY1           PIC  X(100) VALUE SPACE.
           02  TDNWRF_SORT           PIC  X(100) VALUE SPACE.
           02  TDNWRF_IDLST          PIC  X(100) VALUE SPACE.
           02  TDNWRF_RES            USAGE  POINTER.
       01  TDNWR-R.
           02  F                     PIC X(247).
           02  TDNWR-DATE            PIC 9(08).
           02  F                     PIC X(01).
       77  F                         PIC X(01).
      *FD  TDNNRF
       01  TDNNRF_JT200U.
           02  TDNNRF_PNAME1         PIC  X(006) VALUE "TDNNRF".
           02  F                     PIC  X(001).
           02  TDNNRF_LNAME          PIC  X(013) VALUE "TDNNRF_JT200U".
           02  F                     PIC  X(001).
           02  TDNNRF_KEY1           PIC  X(100) VALUE SPACE.
           02  TDNNRF_SORT           PIC  X(100) VALUE SPACE.
           02  TDNNRF_IDLST          PIC  X(100) VALUE SPACE.
           02  TDNNRF_RES            USAGE  POINTER.
       01  TDNNR-R.
           02  TDNNR-D               PIC X(256).
           02  TDNNR-DATE            PIC 9(08).
           02  F                     PIC X(77).
       77  F                         PIC X(01).
      *FD  TDIRF
       01  TDIRF_JT200U.
           02  TDIRF_PNAME1          PIC  X(005) VALUE "TDIRF".
           02  F                     PIC  X(001).
           02  TDIRF_LNAME           PIC  X(012) VALUE "TDIRF_JT200U".
           02  F                     PIC  X(001).
           02  TDIRF_KEY1            PIC  X(100) VALUE SPACE.
           02  TDIRF_SORT            PIC  X(100) VALUE SPACE.
           02  TDIRF_IDLST           PIC  X(100) VALUE SPACE.
           02  TDIRF_RES             USAGE  POINTER.
       01  TDIR-R.
           02  F                     PIC X(246).
           02  TDIR-DATE             PIC 9(08).
           02  F                     PIC X(2).
       77  F                         PIC X(01).
      *FD  TDNARF
       01  TDNARF_JT200U.
           02  TDNARF_PNAME1         PIC  X(006) VALUE "TDNARF".
           02  F                     PIC  X(001).
           02  TDNARF_LNAME          PIC  X(013) VALUE "TDNARF_JT200U".
           02  F                     PIC  X(001).
           02  TDNARF_KEY1           PIC  X(100) VALUE SPACE.
           02  TDNARF_SORT           PIC  X(100) VALUE SPACE.
           02  TDNARF_IDLST          PIC  X(100) VALUE SPACE.
           02  TDNARF_RES            USAGE  POINTER.
       01  TDNAR-R.
           02  F                     PIC X(246).
           02  TDNAR-DATE            PIC 9(08).
           02  F                     PIC X(02).
       77  F                         PIC X(01).
      *****   ÉiÉtÉRêîó ó›êœÉtÉ@ÉCÉã 53/4   ****************************
      *FD  NSURYORF
       01  NSURYORF_JT200U.
           02  NSURYORF_PNAME1  PIC  X(008) VALUE "NSURYORF".
           02  F                PIC  X(001).
           02  NSURYORF_LNAME   PIC  X(015) VALUE "NSURYORF_JT200U".
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
           02  NSURYOR-10       PIC 9(08).
       77  F                    PIC X(01).
      *****   ÉiÉtÉRî†êîó›êœÉtÉ@ÉCÉã 44/5   ****************************
      *FD  NHAKORF
       01  NHAKORF_JT200U.
           02  NHAKORF_PNAME1   PIC  X(007) VALUE "NHAKORF".
           02  F                PIC  X(001).
           02  NHAKORF_LNAME    PIC  X(014) VALUE "NHAKORF_JT200U".
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
           02  NHAKOR-10        PIC 9(08).
       77  F                    PIC X(01).
      *****   ÉiÉtÉRêîó í˘ê≥ÉtÉ@ÉCÉã 128/2  ****************************
      *FD  TEISEIRF
       01  TEISEIRF_JT200U.
           02  TEISEIRF_PNAME1  PIC  X(009) VALUE "NTEISEIRF".
           02  F                PIC  X(001).
           02  TEISEIRF_LNAME   PIC  X(015) VALUE "TEISEIRF_JT200U".
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
           02  TEISEIR-10       PIC 9(08).
       77  F                    PIC X(01).
      *
       77  ESTAT                PIC  X(002).
       77  RESU                 PIC  9(001).
       77  RET                  PIC  9(001) VALUE ZERO.
       77  USER_ID              PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE      PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLEAR.
           02  CLR-GMN  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-MIDAS.
           02  FILLER   PIC  X(13) VALUE
             " ì˙ éü åJ âz ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(27) VALUE
                  "***  NSURYORF WRITE¥◊∞  ***".
             03  E-ME2   PIC  X(26) VALUE
                  "***  NHAKORF WRITE¥◊∞  ***".
           COPY  LSMSG.
           COPY  LIBSCR.
           COPY  LSSEM.
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLEAR
       CALL "SD_Init" USING
           "DSP-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-GMN" "X" "1" "0" "12" " " "DSP-CLEAR" RETURNING RESU.
      *DSP-MIDAS
       CALL "SD_Init" USING
            "DSP-MIDAS" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-MIDAS" "RX" "1" "25" "13" " " "DSP-MIDAS"
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "53" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "10" "27" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "10" "26" "E-ME1" " " RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ******************************************************************
      *                    ÇlÇ`ÇhÇmÅ|ÇqÇsÇm                            *
      ******************************************************************
       MAIN-RTN.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MIDAS" DSP-MIDAS "p" RETURNING RESU.
           MOVE  ZERO         TO  W-DATE.
           ACCEPT    W-DATES  FROM  DATE.
           COPY  LIBCPR.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           PERFORM   DATE-RTN     THRU         DATE-EX.
           PERFORM   OPEN-RTN     THRU         OPEN-EX.
           PERFORM   JSTR-RTN     THRU         JSTR-EX.
           PERFORM   OKJF-RTN     THRU         OKJF-EX.
           PERFORM   JNIF-RTN     THRU         JNIF-EX.
           PERFORM   TDNW-RTN     THRU         TDNW-EX.
           PERFORM   TDNN-RTN     THRU         TDNN-EX.
           PERFORM   TDI-RTN      THRU         TDI-EX.
           PERFORM   TDNA-RTN     THRU         TDNA-EX.
           PERFORM   NSUR-RTN     THRU         NSUR-EX.
           PERFORM   NHAK-RTN     THRU         NHAK-EX.
           PERFORM   NTEI-RTN     THRU         NTEI-EX.
           PERFORM   END-RTN      THRU         END-EX.
           CALL "DB_Close".
           STOP      RUN.
      ******************************************************************
      *                    ÇcÇ`ÇsÇdÅ|ÇqÇsÇm                            *
      ******************************************************************
       DATE-RTN.
           MOVE    ZERO      TO  W-NGP.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "EXCLUSIVE" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
       DATE-010.
      *           READ      JSTR         NEXT               AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     DATE-090
           END-IF
           IF  JSTR-17  NOT =  1
               GO  TO  DATE-010
           END-IF
           IF  JSTR-05      >  W-NGP
               MOVE    JSTR-05   TO  W-NGP
           END-IF
           GO      TO       DATE-010.
       DATE-090.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           IF  W-NGP    NOT =  ZERO
               MOVE    W-NGP     TO  W-DATE
           END-IF.
       DATE-EX.
           EXIT.
      ******************************************************************
      *                    ÇnÇoÇdÇmÅ|ÇqÇsÇm                            *
      ******************************************************************
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "EXCLUSIVE" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "EXCLUSIVE" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNIF_PNAME1 "EXCLUSIVE" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDNWF_PNAME1 "EXCLUSIVE" BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 "EXCLUSIVE" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDIF_PNAME1 "EXCLUSIVE" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "EXCLUSIVE" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "I-O" JT-DNKN_PNAME1 "SHARED" BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           CALL "DB_F_Open" USING
            "INPUT" NSURYOF_PNAME1 " " BY REFERENCE NSURYOF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" NHAKOF_PNAME1 " " BY REFERENCE NHAKOF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TEISEIF_PNAME1 " " BY REFERENCE TEISEIF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" JSTRRF_PNAME1 " " BY REFERENCE JSTRRF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" OKJRF_PNAME1 " " BY REFERENCE OKJRF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" JNIRF_PNAME1 " " BY REFERENCE JNIRF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" TDNWRF_PNAME1 " " BY REFERENCE TDNWRF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" TDNNRF_PNAME1 " " BY REFERENCE TDNNRF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" TDIRF_PNAME1 " " BY REFERENCE TDIRF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" TDNARF_PNAME1 " " BY REFERENCE TDNARF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" NSURYORF_PNAME1 " " BY REFERENCE
            NSURYORF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" NHAKORF_PNAME1 " " BY REFERENCE NHAKORF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" TEISEIRF_PNAME1 " " BY REFERENCE
            TEISEIRF_IDLST "0".
       OPEN-EX.
           EXIT.
      ******************************************************************
      *                    ÇiÇrÇsÇqÅ|ÇqÇsÇm                            *
      ******************************************************************
       JSTR-RTN.
           MOVE  ZERO    TO  WJSTR-01.
       JST-010.
      *           READ  JSTR     NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  JSTR-EX
           END-IF
           IF  JSTR-17  NOT =  1
               GO  TO  JST-015
           END-IF
           IF  JSTR-01      =  WJSTR-01
               GO  TO  JST-020
           END-IF
      ***** íºëóêÊÉ}ÉXÉ^ çXêV
           MOVE  JSTR-06  TO  TC-KEY ERR-K.
      *           READ  TC-M     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  JST-020
           END-IF
           MOVE  JSTR-05S TO  TC-YMD.
      *           REWRITE  TC-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"           TO  ERR-M
               MOVE  "TC-M"        TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  JST-020.
       JST-015.
           IF  JSTR-4021   =  SPACE
               GO  TO  JST-010
           END-IF
           MOVE    SPACE     TO  JSTR-4021.
           MOVE    ZERO      TO  JSTR-4022  JSTR-4023.
      *           REWRITE   JSTR-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"           TO  ERR-M
               MOVE  "JSTR"        TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  JST-010.
       JST-020.
      ***** ì`ï[áÇåüçıÉtÉ@ÉCÉã çXêV
           MOVE  JSTR-07  TO  DNKN-01.
           MOVE  JSTR-09  TO  DNKN-02.
           MOVE  3        TO  DNKN-03.
           MOVE  JSTR-01  TO  DNKN-041.
           MOVE  JSTR-02  TO  DNKN-042.
      *           READ  JT-DNKN  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JT-DNKN_PNAME1 BY REFERENCE DNKN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  JST-030
           END-IF
           MOVE  DNKN-KEY TO  ERR-K.
      *           DELETE  JT-DNKN  INVALID
      *///////////////
           CALL "DB_Delete" USING JT-DNKN_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE     "D"        TO  ERR-M
               MOVE     "JT-DNKN"  TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JST-030.
           INITIALIZE   JSTRR-R.
           MOVE    JSTR-R    TO  JSTRR-R.
           MOVE    W-DATE    TO  JSTRR-90.
      *           WRITE   JSTRR-R.
      *//////////////
           CALL "DB_Insert" USING
            JSTRRF_PNAME1 JSTRRF_LNAME JSTRR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "A"           TO  ERR-M
               MOVE  "JSTRRF"      TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           MOVE    JSTR-KEY  TO  ERR-K.
           MOVE    JSTR-01   TO  WJSTR-01.
      *           DELETE  JSTR  INVALID
      *///////////////
           CALL "DB_Delete" USING JSTR_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"           TO  ERR-M
               MOVE  "JSTR"        TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  JST-010.
       JSTR-EX.
           EXIT.
      ******************************************************************
      *                    ÇnÇjÇiÇeÅ|ÇqÇsÇm          1988.09.22        *
      ******************************************************************
       OKJF-RTN.
      *           READ      OKJF         NEXT               AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     OKJF-EX
           END-IF
      *
           IF  OKJF-02        =   9
               GO      TO       OKJF-DEL
           END-IF
           IF  (OKJF-10        =   1)      AND
               (OKJF-08        =   1)
               GO      TO       OKJF-DEL
           END-IF
      *
           IF  (OKJF-07        <   1)      AND
               (OKJF-10        =   1)
               GO      TO       OKJF-DEL
           END-IF
      *
           IF  OKJF-10    NOT =   0
               GO      TO       OKJF-RTN
           END-IF
           PERFORM   SEN-RTN      THRU         SEN-EX.
           IF  SEN-SW         =   0
               GO      TO       OKJF-DEL
           END-IF
      *
           GO      TO       OKJF-RTN.
       OKJF-DEL.
           INITIALIZE   OKJR-R.
           MOVE    OKJF-R    TO  OKJR-D.
           MOVE    W-DATE    TO  OKJR-DATE.
      *           WRITE   OKJR-R.
      *//////////////
           CALL "DB_Insert" USING
            OKJRF_PNAME1 OKJRF_LNAME OKJR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "A"           TO  ERR-M
               MOVE  "OKJRF"       TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           MOVE  OKJF-KEY   TO  ERR-K.
      *           DELETE    OKJF   INVALID
      *///////////////
           CALL "DB_Delete" USING OKJF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"           TO  ERR-M
               MOVE  "OKJF"        TO  ERR-F
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO    TO   OKJF-RTN.
       OKJF-EX.
           EXIT.
      ******************************************************************
      *                    ÇiÇmÇhÇeÅ|ÇqÇsÇm          1991.11.11        *
      ******************************************************************
       JNIF-RTN.
      *           READ      JNIF         NEXT               AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     JNIF-EX
           END-IF
           IF  JNIF1-02     <     7   AND    JNIF1-10   =   1
               GO  TO  JNIF-DEL
           ELSE
               IF  JNIF1-02     =     7   AND    JNIF2-04   =   1
                   GO  TO  JNIF-DEL
               END-IF
           END-IF
      *
           GO      TO       JNIF-RTN.
       JNIF-DEL.
           INITIALIZE   JNIR-R.
           MOVE    JNIF1-R   TO  JNIR-D.
           MOVE    W-DATE    TO  JNIR-DATE.
      *           WRITE   JNIR-R.
      *//////////////
           CALL "DB_Insert" USING
            JNIRF_PNAME1 JNIRF_LNAME JNIR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "A"           TO  ERR-M
               MOVE  "JNIRF"       TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           MOVE  JNIF1-KEY   TO  ERR-K.
      *           DELETE    JNIF   INVALID
      *///////////////
           CALL "DB_Delete" USING JNIF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"           TO  ERR-M
               MOVE  "JNIF"        TO  ERR-F
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO    TO   JNIF-RTN.
       JNIF-EX.
           EXIT.
      ******************************************************************
       TDNW-RTN.
      *           READ      TDNWF        NEXT               AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     TDNW-EX
           END-IF
           IF  TDNW1-PC     =     9
               IF  TDNW1-HC     =     1
                   GO  TO  TDNW-DEL
               END-IF
           END-IF
           GO      TO       TDNW-RTN.
       TDNW-DEL.
           INITIALIZE   TDNWR-R.
           MOVE    TDNW-R1   TO  TDNWR-R.
           MOVE    W-DATE    TO  TDNWR-DATE.
      *           WRITE   TDNWR-R.
      *//////////////
           CALL "DB_Insert" USING
            TDNWRF_PNAME1 TDNWRF_LNAME TDNWR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "A"           TO  ERR-M
               MOVE  "TDNWRF"      TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           MOVE  TDNW1-KEY   TO  ERR-K.
      *           DELETE    TDNWF  INVALID
      *///////////////
           CALL "DB_Delete" USING TDNWF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"           TO  ERR-M
               MOVE  "TDNWF"       TO  ERR-F
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO    TO   TDNW-RTN.
       TDNW-EX.
           EXIT.
       TDNN-RTN.
      *           READ      TDNNF        NEXT               AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     TDNN-EX
           END-IF
           IF  TDNN1-PC     =     9
               IF  TDNN1-HC     =     1
                   GO  TO  TDNN-DEL
               END-IF
           END-IF
           GO      TO       TDNN-RTN.
       TDNN-DEL.
           INITIALIZE   TDNNR-R.
           MOVE    TDNN-R1   TO  TDNNR-D.
           MOVE    W-DATE    TO  TDNNR-DATE.
      *           WRITE   TDNNR-R.
      *//////////////
           CALL "DB_Insert" USING
            TDNNRF_PNAME1 TDNNRF_LNAME TDNNR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "A"           TO  ERR-M
               MOVE  "TDNNRF"      TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           MOVE  TDNN1-KEY   TO  ERR-K.
      *           DELETE    TDNNF  INVALID
      *///////////////
           CALL "DB_Delete" USING TDNNF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"           TO  ERR-M
               MOVE  "TDNNF"       TO  ERR-F
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO    TO   TDNN-RTN.
       TDNN-EX.
           EXIT.
       TDI-RTN.
      *           READ      TDIF         NEXT               AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     TDI-EX
           END-IF
           IF  TDI-UPC      =     9
               GO  TO  TDI-DEL
           END-IF
           IF  TDI-SOK      =     1
               IF  TDI-PRC      =     9
                   GO  TO  TDI-DEL
               END-IF
           END-IF
           IF  TDI-TCD      =     6010
               IF  TDI-PRC      =     9
                   IF  TDI-UPC      =     1
                       GO  TO  TDI-DEL
                   END-IF
               END-IF
           END-IF
           GO      TO       TDI-RTN.
       TDI-DEL.
           INITIALIZE   TDIR-R.
           MOVE    TDI-R     TO  TDIR-R.
           MOVE    W-DATE    TO  TDIR-DATE.
      *           WRITE   TDIR-R.
      *//////////////
           CALL "DB_Insert" USING
            TDIRF_PNAME1 TDIRF_LNAME TDIR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "A"           TO  ERR-M
               MOVE  "TDIRF"       TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           MOVE  TDI-KEY     TO  ERR-K.
      *           DELETE    TDIF   INVALID
      *///////////////
           CALL "DB_Delete" USING TDIF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"           TO  ERR-M
               MOVE  "TDIF"        TO  ERR-F
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO    TO   TDI-RTN.
       TDI-EX.
           EXIT.
       TDNA-RTN.
      *           READ      TDNAF        NEXT               AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     TDNA-EX
           END-IF
           IF  TDNA-PC      =     9
               GO  TO  TDNA-DEL
           END-IF
           GO      TO       TDNA-RTN.
       TDNA-DEL.
           INITIALIZE   TDNAR-R.
           MOVE    TDNA-R    TO  TDNAR-R.
           MOVE    W-DATE    TO  TDNAR-DATE.
      *           WRITE   TDNAR-R.
      *//////////////
           CALL "DB_Insert" USING
            TDNARF_PNAME1 TDNARF_LNAME TDNAR-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "A"           TO  ERR-M
               MOVE  "TDNARF"      TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           MOVE  TDNA-KEY    TO  ERR-K.
      *           DELETE    TDNAF  INVALID
      *///////////////
           CALL "DB_Delete" USING TDNAF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"           TO  ERR-M
               MOVE  "TDNAF"       TO  ERR-F
               PERFORM  ERR-RTN    THRU  ERR-EX
           END-IF
           GO    TO   TDNA-RTN.
       TDNA-EX.
           EXIT.
       NSUR-RTN.
      *           READ      NSURYOF                         AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NSURYOF_PNAME1 BY REFERENCE NSURYO-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     NSUR-EX
           END-IF.
       NSUR-010.
           INITIALIZE   NSURYOR-R.
           MOVE    NSURYO-R  TO  NSURYOR-R.
           MOVE    W-DATE    TO  NSURYOR-10.
      *           WRITE   NSURYOR-R.
      *//////////////
           CALL "DB_Insert" USING
            NSURYORF_PNAME1 NSURYORF_LNAME NSURYOR-R RETURNING RET.
           IF  ERR-STAT     = "00"
               GO    TO   NSUR-RTN
           END-IF
           IF  ERR-STAT NOT = "34"
               MOVE  "A"           TO  ERR-M
               MOVE  "NSURYORF"    TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NSURYORF_IDLST NSURYORF_PNAME1.
           MOVE "NSURYORF     " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NSURYORF_PNAME1 " " BY REFERENCE
            NSURYORF_IDLST "0".
           GO TO NSUR-010.
       NSUR-EX.
           EXIT.
       NHAK-RTN.
      *           READ      NHAKOF                          AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NHAKOF_PNAME1 BY REFERENCE NHAKO-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     NHAK-EX
           END-IF.
       NHAK-010.
           INITIALIZE   NHAKOR-R.
           MOVE    NHAKO-R   TO  NHAKOR-R.
           MOVE    W-DATE    TO  NHAKOR-10.
      *           WRITE   NHAKOR-R.
      *//////////////
           CALL "DB_Insert" USING
            NHAKORF_PNAME1 NHAKORF_LNAME NHAKOR-R RETURNING RET.
           IF  ERR-STAT     = "00"
               GO    TO   NHAK-RTN
           END-IF
           IF  ERR-STAT NOT = "34"
               MOVE  "A"           TO  ERR-M
               MOVE  "NHAKORF"     TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NHAKORF_IDLST NHAKORF_PNAME1.
           MOVE "NHAKORF      " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NHAKORF_PNAME1 " " BY REFERENCE NHAKORF_IDLST "0".
           GO TO NHAK-010.
       NHAK-EX.
           EXIT.
       NTEI-RTN.
      *           READ      TEISEIF                        AT  END
      *//////////////
           CALL "DB_Read" USING
            "AT END" TEISEIF_PNAME1 BY REFERENCE TEISEI-R " "
            RETURNING RET.
           IF  RET = 1
               GO    TO     NTEI-EX
           END-IF.
       NTEI-010.
           INITIALIZE   TEISEIR-R.
           MOVE    TEISEI-R TO  TEISEIR-R.
           MOVE    W-DATE    TO   TEISEIR-10.
      *           WRITE   TEISEIR-R.
      *//////////////
           CALL "DB_Insert" USING
            TEISEIRF_PNAME1 TEISEIRF_LNAME TEISEIR-R RETURNING RET.
           IF  ERR-STAT     = "00"
               GO    TO   NTEI-RTN
           END-IF
           IF  ERR-STAT NOT = "34"
               MOVE  "A"           TO  ERR-M
               MOVE  "TEISEIRF"   TO  ERR-F
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
      *
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TEISEIRF_IDLST TEISEIRF_PNAME1.
           MOVE "TEISEIRF     " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TEISEIRF_PNAME1 " " BY REFERENCE
            TEISEIRF_IDLST "0".
           GO TO NTEI-010.
       NTEI-EX.
           EXIT.
       SEN-RTN.
           MOVE  0           TO  SEN-SW.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "EXCLUSIVE" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
       SEN-010.
      *           READ  JSTR     NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  SEN-EX
           END-IF
           IF  OKJF-01      NOT =  JSTR-14B
               GO  TO  SEN-010
           END-IF
           MOVE  1           TO  SEN-SW.
       SEN-EX.
           EXIT.
      ******************************************************************
      *                    ÇdÇmÇcÅ|ÇqÇsÇm                              *
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NSURYOF_IDLST NSURYOF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NHAKOF_IDLST NHAKOF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TEISEIF_IDLST TEISEIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE OKJRF_IDLST OKJRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JNIRF_IDLST JNIRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWRF_IDLST TDNWRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNRF_IDLST TDNNRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDIRF_IDLST TDIRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNARF_IDLST TDNARF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NSURYORF_IDLST NSURYORF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NHAKORF_IDLST NHAKORF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TEISEIRF_IDLST TEISEIRF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" NSURYOF_PNAME1 " " BY REFERENCE NSURYOF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" NHAKOF_PNAME1 " " BY REFERENCE NHAKOF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" TEISEIF_PNAME1 " " BY REFERENCE TEISEIF_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE NSURYOF_IDLST NSURYOF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NHAKOF_IDLST NHAKOF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TEISEIF_IDLST TEISEIF_PNAME1.
       END-EX.
           EXIT.
       COPY  LPMSG.

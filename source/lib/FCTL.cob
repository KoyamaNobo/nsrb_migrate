      ************************************************************
      *      < ∫›ƒ€∞Ÿ    Ãß≤Ÿ >        512 REC  /  1 BLK         *
      ************************************************************
       01  FCTL-F.
           02  FCTL-F_PNAME1          PIC  X(007) VALUE "FCONTRL".
           02  F                      PIC  X(001).
           02  FCTL-F_LNAME           PIC  X(006) VALUE "FCTL-F".
           02  F                      PIC  X(001).
           02  FCTL-F_KEY1            PIC  X(100) VALUE SPACE.
           02  FCTL-F_SORT            PIC  X(100) VALUE SPACE.
           02  FCTL-F_IDLST           PIC  X(100) VALUE SPACE.
           02  FCTL-F_RES             USAGE  POINTER.
       01  FCTL-R.
           02  FCTL-REC.
               03  FCTL-KEY               PIC X(06).
               03  FCTL-OLDCD             PIC X(04).
               03  FCTL-NO                PIC 9(02).
               03  FILLER                 PIC X(500).
      *
           02  FCTL-REC1          REDEFINES FCTL-REC.
               03  FCTL-KEY1              PIC X(06).
               03  FCTL-KSMM              PIC 9(02).
               03  FCTL-KONYMD.
                 04  FCTL-KONYY           PIC 9(04).
                 04  FCTL-KONYYD  REDEFINES FCTL-KONYY.
                   05  FCTL-KONYY1        PIC 9(02).
                   05  FCTL-KONYY2        PIC 9(02).
                 04  FCTL-KONMM           PIC 9(02).
                 04  FCTL-KONDD           PIC 9(02).
               03  FCTL-ZENYMD.
                 04  FCTL-ZENYY           PIC 9(04).
                 04  FCTL-ZENYYD  REDEFINES FCTL-ZENYY.
                   05  FCTL-ZENYY1        PIC 9(02).
                   05  FCTL-ZENYY2        PIC 9(02).
                 04  FCTL-ZENMM           PIC 9(02).
                 04  FCTL-ZENDD           PIC 9(02).
               03  FCTL-GESYMD.
                 04  FCTL-GESYY           PIC 9(04).
                 04  FCTL-GESYYD  REDEFINES  FCTL-GESYY.
                   05  FCTL-GESYY1        PIC 9(02).
                   05  FCTL-GESYY2        PIC 9(02).
                 04  FCTL-GESMM           PIC 9(02).
                 04  FCTL-GESDD           PIC 9(02).
               03  FCTL-GEMYMD.
                 04  FCTL-GEMYY           PIC 9(04).
                 04  FCTL-GEMYYD  REDEFINES FCTL-GEMYY.
                   05  FCTL-GEMYY1        PIC 9(02).
                   05  FCTL-GEMYY2        PIC 9(02).
                 04  FCTL-GEMMM           PIC 9(02).
                 04  FCTL-GEMDD           PIC 9(02).
               03  FCTL-ACEPSIN           PIC 9(01).
               03  FCTL-TOUKI.
                 04  FCTL-TOU     OCCURS 15.
                   05  FCTL-TOUF.
                     06  FCTL-TOUFYY      PIC 9(04).
                     06  FCTL-TOUFYYD  REDEFINES FCTL-TOUFYY.
                       07  FCTL-TOUFYY1   PIC 9(02).
                       07  FCTL-TOUFYY2   PIC 9(02).
                     06  FCTL-TOUFMM      PIC 9(02).
                     06  FCTL-TOUFDD      PIC 9(02).
                   05  FCTL-TOUT.
                     06  FCTL-TOUTYY      PIC 9(04).
                     06  FCTL-TOUTYYD  REDEFINES FCTL-TOUTYY.
                       07  FCTL-TOUTYY1   PIC 9(02).
                       07  FCTL-TOUTYY2   PIC 9(02).
                     06  FCTL-TOUTMM      PIC 9(02).
                     06  FCTL-TOUTDD      PIC 9(02).
               03  FCTL-UPDYM.
                 04  FCTL-UPDYY           PIC 9(04).
                 04  FCTL-UPDYYD  REDEFINES FCTL-UPDYY.
                   05  FCTL-UPDYY1        PIC 9(02).
                   05  FCTL-UPDYY2        PIC 9(02).
                 04  FCTL-UPDMM           PIC 9(02).
               03  FCTL-SIMEBI            PIC 9(02).
               03  FILLER                 PIC X(223).
      *    
           02  FCTL-REC2          REDEFINES FCTL-REC.
               03  FCTL-KEY2              PIC X(06).
               03  FCTL-SUB1              PIC 9(01).
               03  FCTL-SUB2              PIC 9(01).
               03  FCTL-SUB3              PIC 9(01).
               03  FCTL-SUB4              PIC 9(01).
               03  FCTL-SUB5              PIC 9(01).
               03  FCTL-SUB6              PIC 9(01).
               03  FILLER                 PIC X(500).
      *    
           02  FCTL-REC3          REDEFINES FCTL-REC.
               03  FCTL-KEY3              PIC X(06).
               03  FCTL-FROM              PIC 9(04).
               03  FCTL-TO                PIC 9(04).
               03  FCTL-PKB               PIC 9(01).
               03  FCTL-PMM               PIC 9(02).
               03  FCTL-KRI               PIC 9(01).
               03  FCTL-SO                PIC 9(01).
               03  FCTL-FROM1             PIC 9(04).
               03  FCTL-TO1               PIC 9(04).
               03  FILLER                 PIC X(485).
      *    
           02  FCTL-REC4          REDEFINES FCTL-REC.
               03  FCTL-KEY4              PIC X(06).
               03  TAX-TBL.                                                 è¡îÔê≈óì
                 04  TAX-AREA    OCCURS  2.
                   05  TAX-FROM.
                     06  TAX-FYY          PIC 9(04).
                     06  TAX-FMM          PIC 9(02).
                     06  TAX-FDD          PIC 9(02).
                   05  TAX-TO.
                     06  TAX-TYY          PIC 9(04).
                     06  TAX-TMM          PIC 9(02).
                     06  TAX-TDD          PIC 9(02).
                   05  TAX-RITU           PIC 9(02)V9(02).                  è¡îÔê≈ó¶
               03  TAX-CODE               PIC 9(04).                        âºï•è¡îÔ
               03  TAX-CODE1              PIC 9(04).                        âºéÛè¡îÔ
               03  F                      PIC X(458).
       77  F                          PIC X(1).

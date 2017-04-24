      ***************************************
      *    ｴﾗｰ DISPLAY (ﾜｰｸ)                *
      ***************************************
       01  DISP-ERR-WORK.
           02  DISP-MSG.
               03  ERR-MSGX.
                   04  ERR-MSGN     PIC N(25).
               03  ERR-SPACE        PIC X(50).
               03  ERR-F            PIC X(12).
               03  ERR-M            PIC X(01).
               03  ERR-K            PIC X(30).
               03  ERR-FLG          PIC X(02).
      *******************************
      *    プリンタ№変更ワーク     *
      *******************************
       01  ASNPRN.
           03  ASNPRN1              PIC  X(03)   VALUE  "PRN".
           03  ASNPRN2              PIC  9(03).
       01  PMEDIA                   PIC  X(06)   VALUE  SPACE.
      *******************************
      *    該当月取込み処理ワーク   *
      *******************************
       01  ZYMD                     PIC  9(08).
       01  ZI                       PIC  9(02).
       01  Z-R.
           02  Z-KEY1               PIC  X(06).
           02  Z-KSMM               PIC  9(02).
           02  Z-KONYMD.
               03  Z-KONYY          PIC  9(04).
               03  Z-KONYYL  REDEFINES Z-KONYY.
                 04  Z-KONYY1       PIC  9(02).
                 04  Z-KONYY2       PIC  9(02).
               03  Z-KONMM          PIC  9(02).
               03  Z-KONDD          PIC  9(02).
           02  Z-ZENYMD.
               03  Z-ZENYY          PIC  9(04).
               03  Z-ZENMM          PIC  9(02).
               03  Z-ZENDD          PIC  9(02).
           02  Z-GESYMD.
               03  Z-GESYY          PIC  9(04).
               03  Z-GESYYL  REDEFINES Z-GESYY.
                 04  Z-GESYY1       PIC  9(02).
                 04  Z-GESYY2       PIC  9(02).
               03  Z-GESMM          PIC  9(02).
               03  Z-GESDD          PIC  9(02).
           02  Z-GEMYMD.
               03  Z-GEMYY          PIC  9(04).
               03  Z-GEMYYL  REDEFINES Z-GEMYY.
                 04  Z-GEMYY1       PIC  9(02).
                 04  Z-GEMYY2       PIC  9(02).
               03  Z-GEMMM          PIC  9(02).
               03  Z-GEMDD          PIC  9(02).
           02  Z-ACEPSIN            PIC  9(01).
           02  Z-TOUKI.
             03  Z-TOU     OCCURS 15.
               04  Z-TOUF.
                 05  Z-TOUFYY       PIC  9(04).
                 05  Z-TOUFYYL  REDEFINES Z-TOUFYY.
                   06  Z-TOUFYY1    PIC  9(02).
                   06  Z-TOUFYY2    PIC  9(02).
                 05  Z-TOUFMM       PIC  9(02).
                 05  Z-TOUFDD       PIC  9(02).
               04  Z-TOUT.
                 05  Z-TOUTYY       PIC  9(04).
                 05  Z-TOUTYYL  REDEFINES Z-TOUTYY.
                   06  Z-TOUTYY1    PIC  9(02).
                   06  Z-TOUTYY2    PIC  9(02).
                 05  Z-TOUTMM       PIC  9(02).
                 05  Z-TOUTDD       PIC  9(02).
           02  Z-UPDYM.
             03  Z-UPDYY            PIC  9(04).
             03  Z-UPDMM            PIC  9(02).
           02  Z-SIMEBI             PIC  9(02).
           02  FILLER               PIC  X(223).

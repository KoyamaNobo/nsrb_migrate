000010***************************************
000020*    ｴﾗｰ DISPLAY (ﾜｰｸ)                *                           85.01.24
000030***************************************
000040 01  DISP-ERR-WORK.
000050     02  DISP-MSG.
000060         03  ERR-MSGX.
000070             04  ERR-MSGN     PIC N(25).
000080         03  ERR-SPACE        PIC X(50).
000090         03  ERR-F            PIC X(12).
000100         03  ERR-M            PIC X(01).
000110         03  ERR-K            PIC X(30).
000120         03  ERR-FLG          PIC X(02).
000130*******************************
000140*    プリンタ№変更ワーク     *
000150*******************************
000160 01  ASNPRN.
000170     03  ASNPRN1              PIC  X(03)   VALUE  "PRN".
000180     03  ASNPRN2              PIC  9(03).
000190 01  PMEDIA                   PIC  X(06)   VALUE  SPACE.
000200*******************************
000210*    該当月取込み処理ワーク   *
000220*******************************
000230*01  ZYMD                     PIC  9(06).                         D.971111
000240 01  ZYMD                     PIC  9(08).                         I.971111
000250 01  ZI                       PIC  9(02).
000260 01  Z-R.
000270     02  Z-KEY1               PIC  X(06).
000280     02  Z-KSMM               PIC  9(02).
000290     02  Z-KONYMD.
000300*****    03  Z-KONYY          PIC  9(02).                         D.971111
000310         03  Z-KONYY          PIC  9(04).                         I.971111
000320         03  Z-KONYYL  REDEFINES Z-KONYY.                         I.971113
000330           04  Z-KONYY1       PIC  9(02).                         I.971113
000340           04  Z-KONYY2       PIC  9(02).                         I.971113
000350         03  Z-KONMM          PIC  9(02).
000360         03  Z-KONDD          PIC  9(02).
000370     02  Z-ZENYMD.
000380*****    03  Z-ZENYY          PIC  9(02).                         D.971111
000390         03  Z-ZENYY          PIC  9(04).                         I.971111
000400         03  Z-ZENMM          PIC  9(02).
000410         03  Z-ZENDD          PIC  9(02).
000420     02  Z-GESYMD.
000430*****    03  Z-GESYY          PIC  9(02).                         D.971111
000440         03  Z-GESYY          PIC  9(04).                         I.971111
000450         03  Z-GESYYL  REDEFINES Z-GESYY.                         I.971113
000460           04  Z-GESYY1       PIC  9(02).                         I.971113
000470           04  Z-GESYY2       PIC  9(02).                         I.971113
000480         03  Z-GESMM          PIC  9(02).
000490         03  Z-GESDD          PIC  9(02).
000500     02  Z-GEMYMD.
000510*****    03  Z-GEMYY          PIC  9(02).                         D.971111
000520         03  Z-GEMYY          PIC  9(04).                         I.971111
000530         03  Z-GEMYYL  REDEFINES Z-GEMYY.                         I.971113
000540           04  Z-GEMYY1       PIC  9(02).                         I.971113
000550           04  Z-GEMYY2       PIC  9(02).                         I.971113
000560         03  Z-GEMMM          PIC  9(02).
000570         03  Z-GEMDD          PIC  9(02).
000580     02  Z-ACEPSIN            PIC  9(01).
000590     02  Z-TOUKI.
000600       03  Z-TOU     OCCURS 15.
000610         04  Z-TOUF.
000620*****      05  Z-TOUFYY       PIC  9(02).                         D.971111
000630           05  Z-TOUFYY       PIC  9(04).                         I.971111
000640           05  Z-TOUFYYL  REDEFINES Z-TOUFYY.                     I.971113
000650             06  Z-TOUFYY1    PIC  9(02).                         I.971113
000660             06  Z-TOUFYY2    PIC  9(02).                         I.971113
000670           05  Z-TOUFMM       PIC  9(02).
000680           05  Z-TOUFDD       PIC  9(02).
000690         04  Z-TOUT.
000700*****      05  Z-TOUTYY       PIC  9(02).                         D.971111
000710           05  Z-TOUTYY       PIC  9(04).                         I.971111
000720           05  Z-TOUTYYL  REDEFINES Z-TOUTYY.                     I.971113
000730             06  Z-TOUTYY1    PIC  9(02).                         I.971113
000740             06  Z-TOUTYY2    PIC  9(02).                         I.971113
000750           05  Z-TOUTMM       PIC  9(02).
000760           05  Z-TOUTDD       PIC  9(02).
000770     02  Z-UPDYM.
000780*****  03  Z-UPDYY            PIC  9(02).                         D.971111
000790       03  Z-UPDYY            PIC  9(04).                         I.971111
000800       03  Z-UPDMM            PIC  9(02).
000810     02  Z-SIMEBI             PIC  9(02).
000820     02  FILLER               PIC  X(223).                        I.971111
000830*****02  FILLER               PIC  X(37).                         D.971111

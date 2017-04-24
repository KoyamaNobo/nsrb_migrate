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
000230 01  ZYMD                     PIC  9(06).
000240 01  ZI                       PIC  9(02).
000250 01  Z-R.
000260     02  Z-KEY1               PIC  X(06).
000270     02  Z-KSMM               PIC  9(02).
000280     02  Z-KONYMD.
000290         03  Z-KONYY          PIC  9(02).
000300         03  Z-KONMM          PIC  9(02).
000310         03  Z-KONDD          PIC  9(02).
000320     02  Z-ZENYMD.
000330         03  Z-ZENYY          PIC  9(02).
000340         03  Z-ZENMM          PIC  9(02).
000350         03  Z-ZENDD          PIC  9(02).
000360     02  Z-GESYMD.
000370         03  Z-GESYY          PIC  9(02).
000380         03  Z-GESMM          PIC  9(02).
000390         03  Z-GESDD          PIC  9(02).
000400     02  Z-GEMYMD.
000410         03  Z-GEMYY          PIC  9(02).
000420         03  Z-GEMMM          PIC  9(02).
000430         03  Z-GEMDD          PIC  9(02).
000440     02  Z-ACEPSIN            PIC  9(01).
000450     02  Z-TOUKI.
000460       03  Z-TOU     OCCURS 15.
000470         04  Z-TOUF.
000480           05  Z-TOUFYY       PIC  9(02).
000490           05  Z-TOUFMM       PIC  9(02).
000500           05  Z-TOUFDD       PIC  9(02).
000510         04  Z-TOUT.
000520           05  Z-TOUTYY       PIC  9(02).
000530           05  Z-TOUTMM       PIC  9(02).
000540           05  Z-TOUTDD       PIC  9(02).
000550     02  Z-UPDYM.
000560       03  Z-UPDYY            PIC  9(02).
000570       03  Z-UPDMM            PIC  9(02).
000580     02  Z-SIMEBI             PIC  9(02).
000590     02  FILLER               PIC  X(37).

000010**************************************
000020*****     入出庫累積ファイル     *****
000030**************************************
000040 FD  JNSR
000050     BLOCK    3     RECORDS
000060     LABEL    RECORD   STANDARD
000070     VALUE     OF  IDENTIFICATION  "JNSR1"
000080     ALTERNATE     IDENTIFICATION  "JNSR2"
000090     ALTERNATE     IDENTIFICATION  "JNSR3".
000100 01  JNSR-R.
000110     02  JNSR-KEY1.                                               KEY1
000120       03  JNSR-01              PIC  9(06)  COMP-3.               品名ｺｰﾄﾞ
000130*****  03  JNSR-02              PIC  9(06)  COMP-3.               D.980515
000140       03  JNSR-02              PIC  9(08)  COMP-3.               I.980515
000150       03  JNSR-03              PIC  9(02).                       入出力区
000160       03  JNSR-04              PIC  9(06)  COMP-3.               伝票№
000170       03  JNSR-05              PIC  9(01).                       行
000180     02  JNSR-06                PIC  9(01).                       倉ｺｰﾄﾞ
000190     02  JNSR-07                PIC  9(01).                       サイズ
000200     02  JNSR-08.                                                 入出庫数
000210       03  JNSR-081             PIC S9(04)  COMP-3    OCCURS  10.
000220     02  JNSR-09                PIC  9(01).                       生産区分
000230     02  JNSR-10                PIC  9(01).                       出荷伝区
000240     02  JNSR-11.
000250       03  JNSR-111             PIC  9(04).                       得意先C
000260       03  JNSR-112             PIC  9(03).                       直送NO
000270     02  JNSR-12                PIC  9(06)  COMP-3.               送り状№
000280     02  JNSR-13                PIC  9(01).                       預り区分
000290     02  JNSR-14                PIC  9(01).                       運送C
000300     02  JNSR-KEY2.
000310       03  JNSR-15.                                               受注
000320         04  JNSR-151           PIC  9(06)  COMP-3.               　受注№
000330         04  JNSR-152           PIC  9(01).                       　行№
000340*****  03  JNSR-16              PIC  9(06)  COMP-3.               D.980515
000350       03  JNSR-16              PIC  9(08)  COMP-3.               I.980515
000360       03  JNSR-17              PIC  9(01).                       ﾚｺｰﾄﾞKBN
000370       03  JNSR-18.                                               伝票
000380         04  JNSR-181           PIC  9(06)  COMP-3.               　伝票№
000390         04  JNSR-182           PIC  9(01).                       　行№
000400     02  JNSR-KEY3.
000410       03  JNSR-19              PIC  9(04).                       得意先C
000420*****  03  JNSR-20              PIC  9(06)  COMP-3.               D.980515
000430       03  JNSR-20              PIC  9(08)  COMP-3.               I.980515
000440       03  JNSR-21              PIC  9(01).                       レコー区
000450       03  JNSR-22.                                               伝票№
000460         04  JNSR-221           PIC  9(06)  COMP-3.               出荷指№
000470         04  JNSR-222           PIC  9(01).                       　行№
000480     02  JNSR-23                PIC  N(09).                       配達
000490     02  JNSR-24                PIC  N(23).                       摘要
000500     02  JNSR-81                PIC  9(08)  COMP-3.               I.040421
000510     02  JNSR-82                PIC  9(03).                       I.040421
000520*****02  FILLER                 PIC  X(11).                       D.980515
000530*****02  FILLER                 PIC  X(08).                       D.040421
000540     02  JNSR-90                PIC  9(01).                       入力部署
000550     02  JNSR-91                PIC  9(01).                       繰越区分
000560     02  JNSR-92                PIC  9(02).                       処理月

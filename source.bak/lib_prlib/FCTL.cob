000010************************************************************
000020*      < ∫›ƒ€∞Ÿ    Ãß≤Ÿ >        512 REC  /  1 BLK         *
000030************************************************************
000040 FD  FCTL-F
000050     BLOCK      CONTAINS     1      RECORDS                       H 90.12
000060     LABEL      RECORD       STANDARD
000070     VALUE      OF           IDENTIFICATION      "FCONTRL".
000080 01  FCTL-REC.
000090     02  FCTL-KEY               PIC X(06).
000100     02  FCTL-OLDCD             PIC X(04).
000110     02  FCTL-NO                PIC 9(02).
000120     02  FILLER                 PIC X(500).                       I.971111
000130*****02  FILLER                 PIC X(244).                       D.971111
000140*
000150 01  FCTL-REC1.
000160     02  FCTL-KEY1              PIC X(06).
000170     02  FCTL-KSMM              PIC 9(02).
000180     02  FCTL-KONYMD.
000190*****  03  FCTL-KONYY           PIC 9(02).                        D.971111
000200       03  FCTL-KONYY           PIC 9(04).                        I.971111
000210       03  FCTL-KONYYD  REDEFINES FCTL-KONYY.                     I.971111
000220         04  FCTL-KONYY1        PIC 9(02).                        I.971111
000230         04  FCTL-KONYY2        PIC 9(02).                        I.971111
000240       03  FCTL-KONMM           PIC 9(02).
000250       03  FCTL-KONDD           PIC 9(02).
000260     02  FCTL-ZENYMD.
000270*****  03  FCTL-ZENYY           PIC 9(02).                        D.971111
000280       03  FCTL-ZENYY           PIC 9(04).                        I.971111
000290       03  FCTL-ZENYYD  REDEFINES FCTL-ZENYY.                     I.971111
000300         04  FCTL-ZENYY1        PIC 9(02).                        I.971111
000310         04  FCTL-ZENYY2        PIC 9(02).                        I.971111
000320       03  FCTL-ZENMM           PIC 9(02).
000330       03  FCTL-ZENDD           PIC 9(02).
000340     02  FCTL-GESYMD.
000350*****  03  FCTL-GESYY           PIC 9(02).                        D.971111
000360       03  FCTL-GESYY           PIC 9(04).                        I.971111
000370       03  FCTL-GESYYD  REDEFINES  FCTL-GESYY.                    I.971111
000380         04  FCTL-GESYY1        PIC 9(02).                        I.971111
000390         04  FCTL-GESYY2        PIC 9(02).                        I.971111
000400       03  FCTL-GESMM           PIC 9(02).
000410       03  FCTL-GESDD           PIC 9(02).
000420     02  FCTL-GEMYMD.
000430*****  03  FCTL-GEMYY           PIC 9(02).                        D.971111
000440       03  FCTL-GEMYY           PIC 9(04).                        I.971111
000450       03  FCTL-GEMYYD  REDEFINES FCTL-GEMYY.                     I.971111
000460         04  FCTL-GEMYY1        PIC 9(02).                        I.971111
000470         04  FCTL-GEMYY2        PIC 9(02).                        I.971111
000480       03  FCTL-GEMMM           PIC 9(02).
000490       03  FCTL-GEMDD           PIC 9(02).
000500     02  FCTL-ACEPSIN           PIC 9(01).
000510     02  FCTL-TOUKI.                                              A 90.12
000520       03  FCTL-TOU     OCCURS 15.                                A 90.12
000530         04  FCTL-TOUF.                                           A 90.12
000540*****      05  FCTL-TOUFYY      PIC 9(02).                        D.971111
000550           05  FCTL-TOUFYY      PIC 9(04).                        I.971111
000560           05  FCTL-TOUFYYD  REDEFINES FCTL-TOUFYY.               I.971111
000570             06  FCTL-TOUFYY1   PIC 9(02).                        I.971111
000580             06  FCTL-TOUFYY2   PIC 9(02).                        I.971111
000590           05  FCTL-TOUFMM      PIC 9(02).                        A 90.12
000600           05  FCTL-TOUFDD      PIC 9(02).                        A 90.12
000610         04  FCTL-TOUT.                                           A 90.12
000620*****      05  FCTL-TOUTYY      PIC 9(02).                        D.971111
000630           05  FCTL-TOUTYY      PIC 9(04).                        I.971111
000640           05  FCTL-TOUTYYD  REDEFINES FCTL-TOUTYY.               I.971111
000650             06  FCTL-TOUTYY1   PIC 9(02).                        I.971111
000660             06  FCTL-TOUTYY2   PIC 9(02).                        I.971111
000670           05  FCTL-TOUTMM      PIC 9(02).                        A 90.12
000680           05  FCTL-TOUTDD      PIC 9(02).                        A 90.12
000690     02  FCTL-UPDYM.
000700*****  03  FCTL-UPDYY           PIC 9(02).                        D.971111
000710       03  FCTL-UPDYY           PIC 9(04).                        I.971111
000720       03  FCTL-UPDYYD  REDEFINES FCTL-UPDYY.                     I.971111
000730         04  FCTL-UPDYY1        PIC 9(02).                        I.971111
000740         04  FCTL-UPDYY2        PIC 9(02).                        I.971111
000750       03  FCTL-UPDMM           PIC 9(02).                        A 90.12
000760     02  FCTL-SIMEBI            PIC 9(02).                        A 91.01
000770     02  FILLER                 PIC X(223).                       I.971111
000780*****02  FILLER                 PIC X(37).                        D.971111
000790*
000800 01  FCTL-REC2.
000810     02  FCTL-KEY2              PIC X(06).
000820     02  FCTL-SUB1              PIC 9(01).
000830     02  FCTL-SUB2              PIC 9(01).
000840     02  FCTL-SUB3              PIC 9(01).
000850     02  FCTL-SUB4              PIC 9(01).
000860     02  FCTL-SUB5              PIC 9(01).
000870     02  FCTL-SUB6              PIC 9(01).
000880     02  FILLER                 PIC X(500).                       I.971111
000890*****02  FILLER                 PIC X(244).                       D.971111
000900*
000910 01  FCTL-REC3.
000920     02  FCTL-KEY3              PIC X(06).
000930     02  FCTL-FROM              PIC 9(04).
000940     02  FCTL-TO                PIC 9(04).
000950     02  FCTL-PKB               PIC 9(01).
000960     02  FCTL-PMM               PIC 9(02).
000970     02  FCTL-KRI               PIC 9(01).
000980     02  FCTL-SO                PIC 9(01).
000990     02  FCTL-FROM1             PIC 9(04).                        A 90.12
001000     02  FCTL-TO1               PIC 9(04).                        A 90.12
001010     02  FILLER                 PIC X(485).                       I.971111
001020*****02  FILLER                 PIC X(229).                       D.971111
001030*
001040 01  FCTL-REC4.
001050     02  FCTL-KEY4              PIC X(06).
001060     02  TAX-TBL.                                                 è¡îÔê≈óì
001070       03  TAX-AREA    OCCURS  2.
001080         04  TAX-FROM.                                            H 90.12
001090*****      05  TAX-FYY          PIC 9(02).                        D.971111
001100           05  TAX-FYY          PIC 9(04).                        I.971111
001110           05  TAX-FMM          PIC 9(02).                        H 90.12
001120           05  TAX-FDD          PIC 9(02).                        H 90.12
001130         04  TAX-TO.                                              A 90.12
001140*****      05  TAX-TYY          PIC 9(02).                        D.971111
001150           05  TAX-TYY          PIC 9(04).                        I.971111
001160           05  TAX-TMM          PIC 9(02).                        A 90.12
001170           05  TAX-TDD          PIC 9(02).                        A 90.12
001180         04  TAX-RITU           PIC 9(02)V9(02).                  è¡îÔê≈ó¶
001190     02  TAX-CODE               PIC 9(04).                        âºï•è¡îÔ
001200     02  TAX-CODE1              PIC 9(04).                        âºéÛè¡îÔ
001210     02  F                      PIC X(458).                       I.971111
001220*****02  F                      PIC X(210).                       D.971111

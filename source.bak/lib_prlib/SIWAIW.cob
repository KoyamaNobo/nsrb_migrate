000010*    仕訳インプット　ワーク   (192/4)
000020 FD  SDW
000030*****BLOCK      CONTAINS     3      RECORDS                       D.000821
000040     BLOCK      CONTAINS     4      RECORDS                       I.000821
000050     LABEL      RECORD       STANDARD
000060     VALUE      OF           IDENTIFICATION      "SIWAKE-IW".
000070 01  SDW-REC.
000080     02  SDWYMD          PIC 9(8).
000090     02  SDWYMD1  REDEFINES SDWYMD.
000100       03  SDWYY         PIC 9(4).
000110       03  SDWMD         PIC 9(4).
000120     02  SDWYMD2  REDEFINES SDWYMD.
000130       03  SDWYM         PIC 9(6).
000140       03  SDWDD         PIC 9(2).
000150     02  SDW-KEY.
000160       03  SDWJNO        PIC 9(6).
000170       03  SDWLNO        PIC 9(2).
000180     02  SDWKARI.
000190       03  KRCDW.
000200         04  KRCDMW      PIC 9(4).
000210         04  KRCDSW      PIC 9(4).
000220       03  KRSECTW       PIC 9(4).
000230       03  KRSKNW        PIC 9(3).
000240       03  KRTAXW        PIC X(1).
000250       03  KRKINW        PIC S9(10).
000260       03  KR-TBW        PIC 9(2).
000270     02  SDWKASI.
000280       03  KSCDW.
000290         04  KSCDMW      PIC 9(4).
000300         04  KSCDSW      PIC 9(4).
000310       03  KSSECTW       PIC 9(4).
000320       03  KSSKNW        PIC 9(3).
000330       03  KSTAXW        PIC X(1).
000340       03  KSKINW        PIC S9(10).
000350       03  KS-TBW        PIC 9(2).
000360     02  SDWCUST         PIC 9(5).
000370     02  SDWTEKICD       PIC 9(3).
000380     02  SDWTEKI         PIC N(20).
000390     02  SDWSIN          PIC X(1).
000400     02  SDWNAMEN        PIC N(10).
000410     02  SDWETAX         PIC X(01).
000420     02  SDWTENO         PIC 9(06).
000430     02  SDWTKD          PIC 9(08).
000440     02  SDWNKCD         PIC 9(02).
000450     02  SDWNSC          PIC 9(01).
000460     02  SDWSKNG         PIC 9(04).
000470     02  SDWSKNGR  REDEFINES SDWSKNG.
000480       03  SDWSKNR       PIC 9(02).
000490       03  SDWSKGR       PIC 9(02).
000500     02  SDWTCD          PIC 9(04).
000510     02  SDWSKD          PIC 9(08).                               I.000821
000520     02  FILLER          PIC X(15).                               I.000821
000530*****02  FILLER          PIC X(01).                               D.000821
000540     02  SDWHHC          PIC 9(01).
000550     02  SDWZHC          PIC 9(01).

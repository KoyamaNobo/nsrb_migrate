000010*    ¶Ó¸ Ï½À°           *
000020 FD  AM
000030     BLOCK      CONTAINS     1      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "KAMOKU-K".
000060 01  AM-REC.
000070     02  AM-KEY.
000080       03  ACCTCD1       PIC 9.
000090       03  ACCTCD2       PIC 9.
000100       03  ACCTCD3       PIC 9.
000110       03  ACCTCD4       PIC 9.
000120     02  DR-CR           PIC 9.
000130*****02  ACCTNM          PIC X(20).                               D901115
000140*****02  CTERM.                                                   D901115
000150*****  03  BFCTZN        PIC S9(11).                              D901115
000160*****  03  CTDR          PIC S9(11).                              D901115
000170*****  03  CTCR          PIC S9(11).                              D901115
000180*****02  MDRCR.                                                   D901115
000190*****  03  BFMZN         PIC S9(11).                              D901115
000200*****  03  MDR           PIC S9(11).                              D901115
000210*****  03  MCR           PIC S9(11).                              D901115
000220     02  DDRCR.
000230       03  BFDZN         PIC S9(11).
000240       03  DDR           PIC S9(11).
000250       03  DCR           PIC S9(11).
000260     02  TEG-BAN         PIC 99.
000270     02  TANA            PIC 9.
000280     02  HOJYO           PIC 9.
000290     02  BS-PL           PIC 9.
000300     02  BSKOU  OCCURS   6   TIMES.
000310       03  BSGOU.
000320         04  BSKEY       PIC 9(3).
000330         04  BSDR-CR     PIC 9.
000340         04  BSCOM       PIC 9.
000350     02  PLKOU  OCCURS   12  TIMES.
000360       03  PLGOU.
000370         04  PLKEY       PIC 9(3).
000380         04  PLCOM       PIC 9.
000390     02  KEIHI           PIC 9.
000400     02  MOTKB           PIC 9.
000410     02  GNKOU  OCCURS   12  TIMES.                               I901115
000420       03  GNGOU.                                                 I901115
000430         04  GNKEY       PIC 9(3).                                I901115
000440         04  GNCOM       PIC 9.                                   I901115
000450     02  SKNKOU.                                                  A 90.12
000460       03  SKNKEY        PIC 9(3).                                A 90.12
000470       03  SKNCOM        PIC 9(1).                                A 90.12
000480       03  SKNHAT        PIC 9(1).                                A 90.12
000490     02  FILLER          PIC X(80).                               H 90.12

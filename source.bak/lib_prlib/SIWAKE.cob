000010*     ¼Ü¹ ÃÞ°À          *     (128/2)
000020 FD  SSD                                                          5807011
000030     BLOCK      CONTAINS     2      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION     "SIWAKE-D".       H 90.12
000060 01  SD-REC.
000070***  02  SD-KEY.                                                  D 90.12
000080*****02  TRDATE          PIC 9(6).                                D.971111
000090     02  KACD1.                                                   I.980225
000100       03  ACCNTCD       PIC 9(4).                                I.980225
000110       03  HOACCNT       PIC 9(4).                                I.980225
000120     02  TRDATE          PIC 9(8).                                I.971111
000130     02  JUNLNO          PIC 9(6).
000140     02  LINENO          PIC 9(2).
000150     02  DR-CR           PIC 9(1).
000160     02  SECTCD          PIC 9(4).                                A 90.12
000170     02  SKINCD          PIC 9(3).                                A 90.12
000180     02  TAXKB           PIC X(1).                                A 90.12
000190***    03  ACCNTCD.                                               D 90.12
000200***      04  ACCTCD1     PIC 9.                                   D 90.12
000210***      04  ACCTCD2     PIC 9.                                   D 90.12
000220***      04  ACCTCD3     PIC 9.                                   D 90.12
000230***      04  ACCTCD4     PIC 9.                                   D 90.12
000240***    03  HOACCNT       PIC 9(3).                                D 90.12
000250***  02  BANKCD          PIC 99.                                  D 90.12
000260***  02  SECTCD          PIC 9(4).                                D 90.12
000270***  02  RATE            PIC 9(4).                                D 90.12
000280     02  AMOUNT          PIC S9(10).
000290     02  TEG-BAN         PIC 9(2).
000300*****02  TEG-NO          PIC X(8).                                D.980225
000310     02  KACD2.
000320       03  OPPCD         PIC 9(4).
000330       03  HOOPPCD       PIC 9(4).                                H 90.12
000340     02  CUSTCD          PIC 9(5).
000350     02  TEKICD          PIC 9(3).                                A 90.12
000360     02  TEKIYO          PIC N(20).                               A 90.12
000370     02  KEIHIKB         PIC 9(1).                                A 90.12
000380*****02  FILLER          PIC X(19).                               D.970729
000390*****02  FILLER          PIC X(18).                               D.971111
000400*****02  FILLER          PIC X(16).                               D.980225
000410     02  NAMEN           PIC N(10).                               I.980225
000420     02  F               PIC X(4).                                I.980225
000430     02  ETAX            PIC X(1).                                I.970729
000440     02  DELKB           PIC X(1).                                A 90.12
000450***  02  TEKIYO          PIC X(20).                               D 90.12
000460***  02  KEIHKB          PIC 9.                                   D 90.12
000470***  02  SKINCD          PIC 9(3).                                D 90.12
000480***  02  USKB            PIC X.                                   D 90.12
000490***  02  FILLER          PIC X(4).                                D 90.12

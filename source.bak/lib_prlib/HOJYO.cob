000010*    Î¼Þ®¶Ó¸ Ï½À°       *
000020 FD  HO
000030     BLOCK      CONTAINS     2      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "HOJYO-M".
000060 01  HO-REC.
000070     02  HO-KEY.
000080       03  H-ACCT        PIC 9(4).
000090       03  H-HOACCT      PIC 9(3).
000100     02  H-DRCR          PIC 9.
000110     02  H-ACCTNM        PIC X(20).
000120     02  H-CTERM.
000130       03  H-BFCTZN      PIC S9(11).
000140       03  H-CTDR        PIC S9(11).
000150       03  H-CTCR        PIC S9(11).
000160     02  H-MDRCR.
000170       03  H-BFMZN       PIC S9(11).
000180       03  H-MDR         PIC S9(11).
000190       03  H-MCR         PIC S9(11).
000200     02  HODDRCR.
000210       03  H-BFDZN       PIC S9(11).
000220       03  H-DDR         PIC S9(11).
000230       03  H-DCR         PIC S9(11).
000240     02  H-MOTKB         PIC 9.

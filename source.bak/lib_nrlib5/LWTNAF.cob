000010 FD  WTNAF
000020     BLOCK  4 RECORDS
000030     LABEL RECORD IS STANDARD
000040     VALUE OF IDENTIFICATION "WTNAF".
000050 01  WTNA-R.
000060     02  WTNA-KEY.
000070       03  WTNA-TEN     PIC  9(004).                              I.100906
000080*****  03  WTNA-TEN     PIC  9(003).                              D.100906
000090     02  WTNA-NAME      PIC  N(026).
000100     02  WTNA-OSN       PIC  9(001).
000110     02  F              PIC  X(007).                              I.100906
000120*****02  F              PIC  X(008).                              D.100906

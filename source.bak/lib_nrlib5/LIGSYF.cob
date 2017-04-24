000010*****************************************
000020*****     生協出荷予定ファイル      *****
000030*****      ( GSYF )  128/2          *****
000040*****************************************
000050 FD  GSYF
000060     BLOCK  2 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "GSYF-RDB".
000090 01  GSY-R.
000100     02  GSY-KEY.
000110       03  GSY-HCD      PIC  9(006).
000120       03  GSY-TCD      PIC  9(004).
000130       03  GSY-ND       PIC  9(008).
000140       03  GSY-SIZ      PIC  9(001).
000150     02  GSY-ASU.
000160       03  GSY-SUD   OCCURS  10.
000170         04  GSY-SU     PIC S9(006).
000180     02  F              PIC  9(002).
000190     02  GSY-BCD1       PIC  9(003).
000200     02  GSY-TEK        PIC  N(016).
000210     02  F              PIC  X(005).
000220     02  GSY-DC         PIC  9(001).
000230     02  GSY-ID         PIC  9(006).

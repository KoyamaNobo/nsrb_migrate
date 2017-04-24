000010***************************************************
000020*****     óöï®àœëıîÃîÑñæç◊ÉtÉ@ÉCÉã            *****
000030*****     (  HIHF  102/5  KEY:1-32)           *****
000040***************************************************
000050 FD  HIHF
000060     BLOCK  5 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HIHF".
000090 01  HIH-R.
000100     02  HIH-KEY.
000110       03  HIH-TCD      PIC  9(004).
000120       03  HIH-HCD      PIC  9(006).
000130       03  HIH-T        PIC  9(005).
000140       03  HIH-DATE     PIC  9(008).
000150       03  HIH-NGP   REDEFINES  HIH-DATE.
000160         04  HIH-NG.
000170           05  HIH-NEN  PIC  9(004).
000180           05  HIH-GET  PIC  9(002).
000190         04  HIH-PEY    PIC  9(002).
000200       03  HIH-NGPD  REDEFINES  HIH-DATE.
000210         04  F          PIC  9(004).
000220         04  HIH-GP     PIC  9(004).
000230       03  HIH-DC       PIC  9(001).
000240       03  HIH-SIZ      PIC  9(001).
000250       03  HIH-DNO      PIC  9(006).
000260       03  HIH-GNO      PIC  9(001).
000270     02  HIH-ASU.
000280       03  HIH-SUD   OCCURS  10.
000290         04  HIH-SU     PIC S9(006).
000300     02  F              PIC  X(010).

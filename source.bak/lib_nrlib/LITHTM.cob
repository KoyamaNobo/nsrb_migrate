000010********************************************
000020*****    得意先品名別　単価ファイル    *****
000030*****    MIX  DATE=THTD (42/8)         *****
000040*****         KEY1=THTM1     (1-11)    *****
000050*****         KEY2=THTM2     (5-11)    *****
000060********************************************
000070 FD  THTM
000080*****BLOCK  8 RECORDS                                             D.080131
000090     BLOCK  6 RECORDS                                             I.080131
000100     LABEL RECORD IS STANDARD
000110     VALUE OF IDENTIFICATION "THTM1"
000120     ALTERNATE IDENTIFICATION "THTM2".
000130 01  THT-R.
000140     02  THT-KD.
000150       03  THT-KEY.
000160         04  THT-TCD    PIC  9(004).
000170         04  THT-HCD.
000180           05  THT-HCDF PIC  9(004).
000190           05  THT-HCDR PIC  9(002).
000200         04  THT-SIZ    PIC  9(001).
000210       03  THT-TCD1     PIC  9(004).
000220     02  THT-KDD   REDEFINES THT-KD.
000230       03  THT-TCD3     PIC  9(004).
000240       03  THT-KEY2.
000250         04  THT-HCD2   PIC  9(006).
000260         04  THT-SIZ2   PIC  9(001).
000270         04  THT-TCD2   PIC  9(004).
000280     02  THT-T          PIC  9(005).
000290     02  F              PIC  X(014).                              I.101020
000300*****02  THT-TT         PIC  9(005).                              D.101020
000310*****02  F              PIC  X(009).                              D.101020
000320     02  THT-TNC        PIC  9(002).
000330*****02  F              PIC  X(004).                              D.080131
000340     02  THT-NG         PIC  9(006).
000350     02  THT-NGL   REDEFINES THT-NG.
000360       03  F            PIC  9(002).
000370       03  THT-NGS      PIC  9(004).

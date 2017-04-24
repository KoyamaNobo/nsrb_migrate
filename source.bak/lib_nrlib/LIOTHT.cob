000010********************************************
000020***** 旧 得意先品名別　単価ファイル    *****
000030*****    MIX  DATE=OTHTD (42/8)        *****
000040*****         KEY1=OTHTM1    (1-11)    *****
000050*****         KEY2=OTHTM2    (5-11)    *****
000060********************************************
000070 FD  OTHTM
000080     BLOCK  6 RECORDS
000090     LABEL RECORD IS STANDARD
000100     VALUE OF IDENTIFICATION "OTHTM1"
000110     ALTERNATE IDENTIFICATION "OTHTM2".
000120 01  OTHT-R.
000130     02  OTHT-KD.
000140       03  OTHT-KEY.
000150         04  OTHT-TCD    PIC  9(004).
000160         04  OTHT-HCD.
000170           05  OTHT-HCDF PIC  9(004).
000180           05  OTHT-HCDR PIC  9(002).
000190         04  OTHT-SIZ    PIC  9(001).
000200       03  OTHT-TCD1     PIC  9(004).
000210     02  OTHT-KDD   REDEFINES OTHT-KD.
000220       03  OTHT-TCD3     PIC  9(004).
000230       03  OTHT-KEY2.
000240         04  OTHT-HCD2   PIC  9(006).
000250         04  OTHT-SIZ2   PIC  9(001).
000260         04  OTHT-TCD2   PIC  9(004).
000270     02  OTHT-T          PIC  9(005).
000280     02  F               PIC  X(014).
000290     02  OTHT-TNC        PIC  9(002).
000300     02  OTHT-NG         PIC  9(006).
000310     02  OTHT-NGL   REDEFINES OTHT-NG.
000320       03  F             PIC  9(002).
000330       03  OTHT-NGS      PIC  9(004).

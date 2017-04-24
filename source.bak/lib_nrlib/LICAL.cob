000010****************************************
000020*****     カレンダー　ファイル     *****
000030*****       ( CALNF )  16/16       *****
000040****************************************
000050 FD  CALNM
000060     BLOCK 16 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "CALNF".
000090 01  CALN-R.
000100     02  CL-KEY.                                                  KEY
000110       03  CL-NG.
000120         04  CL-NEN     PIC  9(004).                              ﾈﾝ
000130         04  CL-GET     PIC  9(002).                              ﾂｷ
000140       03  CL-PEY       PIC  9(002).                              ﾋ
000150     02  CL-KEYD  REDEFINES CL-KEY.
000160       03  CL-NEND      PIC  9(004).                              ﾈﾝ
000170       03  CL-GP.
000180         04  CL-GETD    PIC  9(002).                              ﾂｷ
000190         04  CL-PEYD    PIC  9(002).                              ﾋ
000200     02  CL-NGP   REDEFINES  CL-KEY.                              I.970909
000210       03  F            PIC  9(002).                              I.970909
000220       03  CL-NGPS      PIC  9(006).                              I.970909
000230     02  CL-DATE  REDEFINES  CL-KEY  PIC  9(008).
000240     02  CL-YB          PIC  9(001).                              ﾖｳﾋﾞ
000250*****02  CL-HO          PIC  9(001).                              D.020205
000260     02  F              PIC  9(001).                              I.020205
000270     02  CL-SJ          PIC  9(001).                              BKｷｭｳｼﾞﾂ
000280     02  CL-AHO.                                                  I.020205
000290       03  CL-HOD   OCCURS   5.                                   I.020205
000300         04  CL-H       PIC  9(001).                              I.020205
000310*****02  F              PIC  X(005).                              D.020205

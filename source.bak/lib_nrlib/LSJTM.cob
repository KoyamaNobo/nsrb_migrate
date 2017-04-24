000010****************************************
000020*****      材料統計　ワーク        *****
000030*****   (  WK0128NNN  128/2  )     *****
000040****************************************
000050 FD  JT-F
000060     BLOCK  2 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION WK0128ID.
000090 01  JT-R.
000100     02  JT-KEY.                                                   ＫＥＹ
000110       03  JT-KEYD.
000120         04  JT-BC      PIC  9(001).                              　　部 
000130         04  JT-RC      PIC  9(002).                                  品 
000140       03  JT-JC        PIC  9(003).
000150     02  JT-JCD  REDEFINES JT-KEY  PIC  9(006).                     材料ｺｰ
000160     02  JT-TSU         PIC S9(007)V9(02).
000170     02  JT-SSU         PIC S9(007)V9(02).                        入庫数
000180     02  JT-SIK         PIC S9(008).
000190     02  JT-HSU         PIC S9(007)V9(02).                        出庫数
000200     02  JT-ZKS         PIC S9(007)V9(02).                        繰越数
000210     02  JT-ZKK         PIC S9(008).
000220     02  JT-YC          PIC  9(001).                              用途区 
000230     02  JT-ZC          PIC  9(001).                              在庫区 
000240     02  JT-SC          PIC  9(001).                              製品区 
000250     02  JT-CSU         PIC S9(007)V9(02).
000260     02  JT-BKC         PIC  9(002).
000270     02  JT-BKNO        PIC  9(002).
000280     02  F              PIC  X(011).
000290     02  F              PIC  X(043).

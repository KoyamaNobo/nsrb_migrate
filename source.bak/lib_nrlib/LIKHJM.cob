000010***********************************************
000020*****     �H�i�i���ޗ��g�p�}�X�^�[�@      *****
000030*****         (  KHJM  32/8  )            *****
000040***********************************************
000050 FD  KHJ-M
000060     BLOCK  8 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "KHJM".
000090 01  KHJ-R.
000100     02  KHJ-KEY.                                                 ����
000110       03  KHJ-HCD      PIC  X(005).                              ��ҲC
000120       03  KHJ-JCD      PIC  X(007).                              �޲خ�C
000130*
000140     02  KHJ-SGR        PIC  9(004)V9(01).                        ��ѼֳG
000150     02  KHJ-SGRD  REDEFINES KHJ-SGR  PIC  9(001)V9(04).          ��ѼֳG
000160*
000170     02  KHJ-SU         PIC  9(001).                              �Ÿ޺��
000180     02  KHJ-KSC        PIC  9(001).                              �Ÿ޼ֳC
000190*
000200     02  F              PIC  X(013).

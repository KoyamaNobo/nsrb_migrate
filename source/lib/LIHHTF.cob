      *********************************************
      *****     �����i�����v�}�X�^�[�@�@      *****
      *****      ( HHTFD   256/1 )            *****
      *****      ( HHTF1   7-7   )            *****
      *****      ( HHTF2   1-13  )            *****
      *********************************************
       01  HHTF.
           02  HHTF_PNAME1    PIC  X(005) VALUE "HHTF1".
           02  F              PIC  X(001).
           02  HHTF_PNAME2    PIC  X(005) VALUE "HHTF2".
           02  F              PIC  X(001).
           02  HHTF_LNAME     PIC  X(004) VALUE "HHTF".
           02  F              PIC  X(001).
           02  HHTF_KEY1      PIC  X(100) VALUE SPACE.
           02  HHTF_KEY2      PIC  X(100) VALUE SPACE.
           02  HHTF_SORT      PIC  X(100) VALUE SPACE.
           02  HHTF_IDLST     PIC  X(100) VALUE SPACE.
           02  HHTF_RES       USAGE  POINTER.
       01  HHT-R.
      *    ----- �j�d�x --------------------------------------------------------
           02  HHT-KEY2.
             03  HHT-MHCD     PIC  9(006).
             03  HHT-KEY.
               04  HHT-HCD    PIC  9(006).
               04  HHT-HCDD  REDEFINES HHT-HCD.
                 05  HHT-HCD1 PIC  9(004).
                 05  HHT-HCD2 PIC  9(002).
               04  HHT-SIZ    PIC  9(001).
      *    ----- ���v���� ------------------------------------------------------
           02  HHT-AZSU.                                                �O���c��
             03  HHT-ZSUD  OCCURS  10.
               04  HHT-ZSU    PIC S9(006) COMP-3.
           02  HHT-ANSU.                                                ���ɐ�
             03  HHT-NSUD  OCCURS  10.
               04  HHT-NSU    PIC S9(006) COMP-3.
           02  HHT-AUSU.                                                �o�ɐ�
             03  HHT-USUD  OCCURS  10.
               04  HHT-USU    PIC S9(006) COMP-3.
           02  HHT-AASS.                                                �a��o��
             03  HHT-ASSD  OCCURS  10.
               04  HHT-ASS    PIC S9(004) COMP-3.
           02  HHT-ATZS.                                                �I������
             03  HHT-TSZD  OCCURS  10.
               04  HHT-TZS    PIC S9(006) COMP-3.
           02  HHT-ATSU.                                                �I����
             03  HHT-TSUD  OCCURS  10.
               04  HHT-TSU    PIC S9(006) COMP-3.
      *    ----- ���@�� --------------------------------------------------------
           02  HHT-BCD12.
             03  HHT-BCD1     PIC  9(003).
             03  HHT-BCW1  REDEFINES HHT-BCD1.
               04  HHT-BC1    PIC  9(002).
               04  HHT-BC21   PIC  9(001).
             03  HHT-BC22     PIC  9(001).
           02  HHT-BCW12 REDEFINES HHT-BCD12.
             03  F            PIC  9(002).
             03  HHT-BC2      PIC  9(002).
           02  HHT-BC3        PIC  9(002).                              ����CD3
           02  HHT-BCD3  REDEFINES HHT-BC3.
             03  HHT-BC31     PIC  9(001).
             03  HHT-BC32     PIC  9(001).
           02  HHT-BMNO       PIC  9(001).
           02  HHT-BC4        PIC  9(001).
      *
           02  F              PIC  X(005).
       77  F                  PIC X(1).

      ******************************************
      *****                                *****
      *****     ��@�s�@�}�@�X�@�^�@�[     *****
      *****      ( B A N K M )   85/3      *****
      ******************************************
       01  BANK-M.
           02  BANK-M_PNAME1  PIC  X(005) VALUE "BANKM".
           02  F              PIC  X(001).
           02  BANK-M_LNAME   PIC  X(006) VALUE "BANK-M".
           02  F              PIC  X(001).
           02  BANK-M_KEY1    PIC  X(100) VALUE SPACE.
           02  BANK-M_SORT    PIC  X(100) VALUE SPACE.
           02  BANK-M_IDLST   PIC  X(100) VALUE SPACE.
           02  BANK-M_RES     USAGE  POINTER.
       01  BANK-R.
           02  B-KEY          PIC  X(004).                              �j�d�x
           02  B-BNA          PIC  N(008).                              ��s��
           02  B-SNA          PIC  N(008).                              �{�x�X��
           02  B-YBW          PIC  9(010).                              �����g
           02  B-ZYZ          PIC  9(010).                              �O�����c
           02  B-YBZ          PIC  9(010).                              �����c��
           02  B-YBC          PIC  9(001).                              �����敪
           02  F              PIC  X(011).
           02  B-SC           PIC  9(001).                              �g�p�敪
           02  B-NG           PIC  9(004).                              �ŏI�N��
           02  B-PRC          PIC  9(002).                              ��\�敪
       77  F                  PIC X(1).

      ********************************
      *****  ���ɗ\��c�t�@�C��  *****
      *****       85/3  IS       *****
      ********************************
       01  JNYZ.
           02  JNYZ_PNAME1        PIC  X(004) VALUE "JNYZ".
           02  F                  PIC  X(001).
           02  JNYZ_LNAME         PIC  X(004) VALUE "JNYZ".
           02  F                  PIC  X(001).
           02  JNYZ_KEY1          PIC  X(100) VALUE SPACE.
           02  JNYZ_KEY2          PIC  X(100) VALUE SPACE.
           02  JNYZ_SORT          PIC  X(100) VALUE SPACE.
           02  JNYZ_IDLST         PIC  X(100) VALUE SPACE.
           02  JNYZ_RES           USAGE  POINTER.
      *
       01  JNYZ-R.
           02   JNYZ-KEY.                                               KEY
                03    JNYZ-01     PIC 9(6).                             �ݺ���
                03    JNYZ-02     PIC 9(1).                             ���޸���
           02   JNYZ-03.                                                ���ò���
                03  JNYZ-031      OCCURS  10.                           �������
                    04  JNYZ-0311    PIC S9(6).
           02   F                 PIC X(16).
           02   JNYZ-99           PIC 9(2).                             ·
       77  F                      PIC X(01).

       01  NJZAI.                                                        ����»޲�Ͻ��
           02  NJZAI_PNAME1       PIC  X(005) VALUE "NJZAI".
           02  F                  PIC  X(001).
           02  NJZAI_LNAME        PIC  X(005) VALUE "NJZAI".
           02  F                  PIC  X(001).
           02  NJZAI_KEY1         PIC  X(100) VALUE SPACE.
           02  NJZAI_KEY2         PIC  X(100) VALUE SPACE.
           02  NJZAI_KEY3         PIC  X(100) VALUE SPACE.
           02  NJZAI_SORT         PIC  X(100) VALUE SPACE.
           02  NJZAI_IDLST        PIC  X(100) VALUE SPACE.
           02  NJZAI_RES          USAGE  POINTER.
      *
       01  NJZAI-R.
           02   NJZAI-KEY.                                              KEY
                03    NJZAI-01    PIC 9(1).                             �� ����
                03    NJZAI-02    PIC 9(6).                             �ݺ���
                03    NJZAI-03    PIC 9(1).                             ���޸���
           02   NJZAI-04.                                               ��ݸغ�
                03  NJZAI-041     OCCURS  10.                           �������
                    04  NJZAI-0411   PIC S9(6)     COMP-3.
           02   NJZAI-05.                                               ĳƭ��
                03  NJZAI-051     OCCURS  10.                           �������
                    04  NJZAI-0511   PIC S9(6)     COMP-3.
           02   NJZAI-06.                                               ĳ����
                03  NJZAI-061     OCCURS  10.                           �������
                    04  NJZAI-0611   PIC S9(6)     COMP-3.
           02   NJZAI-07.
                03  NJZAI-071     OCCURS  10.                           �������
                    04  NJZAI-0711   PIC S9(6)     COMP-3.
           02   NJZAI-08.
                03  NJZAI-081     OCCURS  10.                           �������
                    04  NJZAI-0811   PIC S9(6)     COMP-3.
           02   NJZAI-09.                                               ����ж��
                03  NJZAI-091     OCCURS  10.                           �������
                    04  NJZAI-0911   PIC S9(6)     COMP-3.
           02   NJZAI-10.                                               �޲�����
                03  NJZAI-101     OCCURS  10.                           �������
                    04  NJZAI-1011   PIC S9(6)     COMP-3.
           02   NJZAI-11.                                               �Һ�ƭ��
                03  NJZAI-111     OCCURS  10.                           �������
                    04  NJZAI-1111   PIC S9(6)     COMP-3.
           02   FILLER            PIC X(12).
           02   NJZAI-99          PIC X(01).                            �޳��C
       77  F                      PIC  X(001).

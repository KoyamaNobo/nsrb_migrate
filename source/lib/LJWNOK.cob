       01  JWNOK.                                                       ɳ���¼����òܰ�
           02  JWNOK_PNAME1          PIC  X(009) VALUE SPACE.
           02  F                     PIC  X(001).
           02  JWNOK_LNAME           PIC  X(005) VALUE "JWNOK".
           02  F                     PIC  X(001).
           02  JWNOK_KEY1            PIC  X(100) VALUE SPACE.
           02  JWNOK_SORT            PIC  X(100) VALUE SPACE.
           02  JWNOK_IDLST           PIC  X(100) VALUE SPACE.
           02  JWNOK_RES             USAGE  POINTER.
      *
       01  JWNOK-R.
           02   JWNOK-KEY.                                              ���  KEY
                03    JWNOK-01.                                         ɳ�
                      04  JWNOK-011  PIC 9(4).
                      04  JWNOK-012  PIC 9(2).                          ·
                      04  JWNOK-013  PIC 9(2).                          �
                03  JWNOK-02.                                           ������C
                    04  JWNOK-021    PIC 9(4).                          ĸ�����
                    04  JWNOK-022    PIC 9(3).                          �����NO
                03  JWNOK-03         PIC 9(6).                          �ݺ���
                03  JWNOK-04         PIC 9(6).                          �ޭ���NO
                03  JWNOK-05         PIC 9(1).                          ���޸���
           02   JWNOK-06             PIC 9(1).                          ��޶ظ��
           02   JWNOK-07.                                               �ޭ���
                03  JWNOK-071    OCCURS  10.                            �������
                    04  JWNOK-0711   PIC S9(6).
           02   JWNOK-08             PIC 9(4).
           02   JWNOK-09             PIC 9(1).
           02   FILLER               PIC X(162).
       77  F                         PIC X(001).

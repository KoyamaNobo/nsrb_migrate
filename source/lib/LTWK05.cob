       01  JT-WK05.
           02  JT-WK05_PNAME1         PIC  X(009) VALUE SPACE.
           02  F                      PIC  X(001).
           02  JT-WK05_LNAME          PIC  X(007) VALUE "JT-WK05".
           02  F                      PIC  X(001).
           02  JT-WK05_KEY1           PIC  X(100) VALUE SPACE.
           02  JT-WK05_SORT           PIC  X(100) VALUE SPACE.
           02  JT-WK05_IDLST          PIC  X(100) VALUE SPACE.
           02  JT-WK05_RES            USAGE  POINTER.
      *
       01  WK05-R.
           02   WK05-01               PIC 9(1).                         ��޶ظ��
           02   WK05-KEY.
                03   WK05-06.                                           �ޭ���
                     04  WK05-061     PIC 9(6).                         �ޭ���NO
                     04  WK05-062     PIC 9(1).                         �ޮ�
                03   WK05-03.                                           ������
                     04  WK05-031     PIC 9(4).
                     04  WK05-032     PIC 9(2).                         ·
                     04  WK05-033     PIC 9(2).                         �
                03   WK05-030   REDEFINES  WK05-03  PIC 9(8).
                03   WK05-02.                                           ����
                     04   WK05-021    PIC 9(6).                         ��������
                     04   WK05-022    PIC 9(1).                         �ޮ�
           02   WK05-04.
                03  WK05-041          PIC 9(4).                         ĸ�����
                03  WK05-042          PIC 9(3).                         ����� NO
           02   WK05-07               PIC 9(6).                         �ݺ���
           02   WK05-08               PIC 9(1).                         ���޸���
           02   WK05-09.                                                ������
                03  WK05-091    OCCURS  10.                             �������
                    04  WK05-0911     PIC S9(4)   COMP-3.
                03  WK05-092          PIC S9(6)   COMP-3.               ��
           02   WK05-10               PIC X(1).                         ��ݸ
           02   FILLER                PIC X(29).
           02   WK05-94               PIC 9(3).                         �o�ג���
           02   WK05-95               PIC X(6).                         �󒍕i��
           02   WK05-96.                                                �󒍓�
                03  WK05-961          PIC 9(4).
                03  WK05-961L  REDEFINES  WK05-961.
                    04  WK05-9611     PIC 9(2).
                    04  WK05-9612     PIC 9(2).
                03  WK05-962          PIC 9(2).                         �@���@
                03  WK05-963          PIC 9(2).                         �@��
           02   WK05-97               PIC 9(5).
           02   WK05-98               PIC S9(3).                        �Z�b�g��
           02   WK05-99               PIC 9(2).                         ���x
       77  F                          PIC X(1).

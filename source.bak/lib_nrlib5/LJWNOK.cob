000010 FD  JWNOK                                                        ɳ����
000020     BLOCK    1     RECORDS                                       �����ò
000030     LABEL    RECORD   STANDARD                                   ܰ�
000040     VALUE    OF  IDENTIFICATION   WK0256ID.                      I.960208
000050*****VALUE    OF  IDENTIFICATION   JT-OWS256ID.                   D.960208
000060*
000070 01  JWNOK-R.
000080     02   JWNOK-KEY.                                              ���  KEY
000090          03    JWNOK-01.                                         ɳ�
000100*****           04  JWNOK-011   PIC 9(2).                         D.980515
000110                04  JWNOK-011   PIC 9(4).                         I.980515
000120                04  JWNOK-012   PIC 9(2).                         ·
000130                04  JWNOK-013   PIC 9(2).                         �
000140          03  JWNOK-02.                                           ������C
000150              04  JWNOK-021    PIC 9(4).                          ĸ�����
000160              04  JWNOK-022    PIC 9(3).                          �����NO
000170          03  JWNOK-03         PIC 9(6).                          �ݺ���
000180          03  JWNOK-04         PIC 9(6).                          �ޭ���NO
000190          03  JWNOK-05         PIC 9(1).                          ���޸���
000200     02   JWNOK-06             PIC 9(1).                          ��޶ظ��
000210     02   JWNOK-07.                                               �ޭ���
000220          03  JWNOK-071    OCCURS  10.                            �������
000230              04  JWNOK-0711   PIC S9(6).
000240*****02   FILLER               PIC X(15).                         D.941019
000250     02   JWNOK-08             PIC 9(4).                          I.941019
000260     02   JWNOK-09             PIC 9(1).                          I.941019
000270     02   FILLER               PIC X(162).                        I.980515
000280*****02   FILLER               PIC X(10).                         D.980515
000290*****02   FILLER               PIC X(154).                        D.980515

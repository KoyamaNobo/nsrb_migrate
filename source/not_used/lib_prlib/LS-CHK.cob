000010 CHK-1.
000020     GO TO      CHK-4.
000030     OPEN       INPUT        SM.
000040     MOVE       "1111"       TO       SM-KEY.
000050     READ       SM           INVALID KEY      GO TO CHK-2.
000060     IF         S-800        NOT =    " "     GO TO CHK-2.
000070     GO TO CHK-3.
000080 CHK-2.
000090     DISPCRT   (3,5)   "����۸��� � ��� �޷Ͼ�"
000100               (4,5)   "Ĳ�ܾ �� ... ����� �ޮ�γ�� ���޲��"
000110               (5,5)   "TEL ... 03-454-5111 (510)  ���޳ ���".
000120     CLOSE     SM.
000130     ACEPCRT   (6,5)   S-800.
000140     STOP      RUN.
000150 CHK-3.
000160     CLOSE     SM.
000170 CHK-4.
000180 END

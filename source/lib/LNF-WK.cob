      ***********************************
      ******    �׎D���[�N�@�@�@�@�@�@�@*
      ******                SEQ         *
      ******                128/2       *
      ***********************************
       01  NF-WK.
           02  NF-WK_PNAME1            PIC  X(017) VALUE SPACE.
           02  F                       PIC  X(001).
           02  NF-WK_LNAME             PIC  X(005) VALUE "NF-WK".
           02  F                       PIC  X(001).
           02  NF-WK_KEY1              PIC  X(100) VALUE SPACE.
           02  NF-WK_SORT              PIC  X(100) VALUE SPACE.
           02  NF-WK_IDLST             PIC  X(100) VALUE SPACE.
           02  NF-WK_RES               USAGE  POINTER.
      *
       01  NF-R.
           02  NF-R1.
               03   NF1-01                 PIC 9(06).                   ����
               03   NF1-02.                                             �}��
                    04   NF1-021           PIC 9(02).                   �@����
                    04   NF1-022           PIC 9(03).                   �@�A��
               03   NF1-03                 PIC 9(01).                   �s
               03   NF1-04.                                             ������
                    04  NF1-041            PIC 9(02).                   �@�N
                    04  NF1-042            PIC 9(02).                   �@��
                    04  NF1-043            PIC 9(02).                   �@��
               03   NF1-05                 PIC 9(06).                   �i����
               03   NF1-06.
                    04  NF1-061            PIC 9(04).                   ���Ӑ�CD
                    04  NF1-062            PIC 9(03).                   ������CD
               03   NF1-07                 PIC 9(01).                   �^���Ǝ�
               03   NF1-08                 PIC 9(01).                   �q ����
               03   NF1-09                 PIC S9(03).                  ��
               03   NF1-10.                                             �o�א�
                    04 NF1-101             PIC S9(03)   OCCURS  27.     �T�C�Y��
               03   NF1-11                 PIC S9(03).                  ����
               03   NF1-12                 PIC 9(01).                   ��ʋ���
               03   NF1-13.                                             ������CD
                    04  NF1-131            PIC 9(04).                   ���Ӑ�CD
                    04  NF1-132            PIC 9(03).                   ������CD
           02  NF-R2    REDEFINES  NF-R1.
               03   NF2-01                 PIC 9(06).                   ����
               03   NF2-02.                                             �}��
                    04   NF2-021           PIC 9(02).                   �@����
                    04   NF2-022           PIC 9(03).                   �@�A��
               03   NF2-03                 PIC 9(01).                   �s
               03   NF2-04                 PIC N(09).                   �z�B
               03   NF2-05                 PIC N(23).                   �E�v
               03   F                      PIC X(44).
               03   NF2-12                 PIC 9(01).                   ��ʋ���
               03   NF2-99.                                             ������CD
                    04  NF2-991            PIC 9(04).                   ���Ӑ�CD
                    04  NF2-992            PIC 9(03).                   ������CD
       77  F                           PIC X(01).

      ***************************************
      *****     �ށ@���@�}�@�X�@�^�@�[  *****
      *****        (  JM  128/2  )      *****
      ***************************************
       01  J-M.
           02  J-M_PNAME1     PIC  X(002) VALUE "JM".
           02  F              PIC  X(001).
           02  J-M_LNAME      PIC  X(003) VALUE "J-M".
           02  F              PIC  X(001).
           02  J-M_KEY1       PIC  X(100) VALUE SPACE.
           02  J-M_SORT       PIC  X(100) VALUE SPACE.
           02  J-M_IDLST      PIC  X(100) VALUE SPACE.
           02  J-M_RES        USAGE  POINTER.
       01  J-R.
           02  J-KEY.                                                   �j�d�x
             03  J-KEYD.
               04  J-BC       PIC  9(001).                              �@�@����
               04  J-RC       PIC  9(002).                                  �i��
             03  J-JC         PIC  9(003).
           02  J-JCD   REDEFINES J-KEY  PIC 9(006).                     �ޗ�����
           02  J-NAME         PIC  N(024).                              �ޗ���
           02  J-YC           PIC  9(001).                              �p�r�敪
           02  J-ZC           PIC  9(001).                              �݌ɋ敪
           02  J-SC           PIC  9(001).                              ���i�敪
           02  J-TC1          PIC  9(001).                              �P�ʋ敪
           02  J-ST           PIC S9(006)V9(02).                           �ŏI�P��
           02  J-TC2          PIC  9(001).                              �P�ʋ敪
           02  J-YT           PIC S9(006)V9(02).                           �\��P��
           02  J-MCD          PIC  9(006).                              ���H�OCD
           02  J-KT           PIC  9(006)V9(02).                           ���H�P��
           02  FILLER         PIC  X(022).                              FILLER
           02  J-BKC          PIC  9(002).                              ����Ǘ�
           02  J-BKNO         PIC  9(002).
           02  F              PIC  X(007).
           02  J-ED           PIC  9(006).                              �ŏI���t
           02  J-EDR   REDEFINES J-ED.
               03  J-EDY      PIC  9(002).                              �@�N
               03  J-EDM      PIC  9(002).                              �@��
               03  J-EDD      PIC  9(002).                              �@��
       77  F                  PIC  X(001).

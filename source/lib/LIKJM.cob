      ***********************************************
      *****     �h�U�@�ޗ��}�X�^�[�@�@�@�@�@    *****
      *****      (  KJM  128/2  )               *****
      ***********************************************
       01  KJ-M.
           02  KJ-M_PNAME1    PIC  X(003) VALUE "KJM".
           02  F              PIC  X(001).
           02  KJ-M_LNAME     PIC  X(004) VALUE "KJ-M".
           02  F              PIC  X(001).
           02  KJ-M_KEY1      PIC  X(100) VALUE SPACE.
           02  KJ-M_SORT      PIC  X(100) VALUE SPACE.
           02  KJ-M_IDLST     PIC  X(100) VALUE SPACE.
           02  KJ-M_RES       USAGE  POINTER.
       01  KJ-R.
      *    * * *   � � �  � � � �   * * *
           02  KJ-KEY.                                                  ����
             03  KJ-JCD       PIC  X(007).
           02  KJ-NAME        PIC  X(020).                              �޲خ�Ҳ
           02  KJ-SC          PIC  9(001).                              ��޲����
      *    * * *   � � �  � � � �   * * *
           02  KJ-T           PIC  9(005)V9(02).                            �ݶ ĳ��
      *    * * *   � � � �  � � � �   * * *
           02  KJ-SSU         PIC S9(007).                              ��ڽ�
           02  KJ-KSU         PIC S9(005).                              �Ÿ޶���
           02  KJ-HSU         PIC S9(007).                              �ײ�޼��
           02  KJ-ZSU         PIC S9(007).                              �غ���
      *    * * *   � � � � �  � � � �   * * *
           02  KJ-JTS         PIC S9(007).                              �����
           02  KJ-TTS         PIC S9(007).                              �������
      *    * * *   ���ݼ�� � ��Ҳ����   * * *
           02  KJ-HCD         PIC  X(005).
      *
           02  F              PIC  X(044).
           02  KJ-ENG         PIC  9(004).
       77  F                  PIC  X(001).
      *

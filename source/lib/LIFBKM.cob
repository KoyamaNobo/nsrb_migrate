      ******************************************************
      *****                                            *****
      *****     �U�@���@��@��@�s�@�}�@�X�@�^�@�[     *****
      *****            ( F B K M )     64/4            *****
      ******************************************************
       01  FBKM.
           02  FBKM_PNAME1    PIC  X(004) VALUE "FBKM".
           02  F              PIC  X(001).
           02  FBKM_LNAME     PIC  X(004) VALUE "FBKM".
           02  F              PIC  X(001).
           02  FBKM_KEY1      PIC  X(100) VALUE SPACE.
           02  FBKM_SORT      PIC  X(100) VALUE SPACE.
           02  FBKM_IDLST     PIC  X(100) VALUE SPACE.
           02  FBKM_RES       USAGE  POINTER.
       01  FBK-R.
           02  FBK-KEY.                                                 �R�[�h
             03  FBK-BKC      PIC  9(004).                              ��s����
             03  FBK-HSC      PIC  9(003).                              �x�X����
           02  FBK-BNA.
             03  FBK-BKN      PIC  X(015).                              ��s��
             03  FBK-HSN      PIC  X(015).                              �{�x�X��
           02  F              PIC  X(001).
           02  FBK-ENG        PIC  9(004).
       77  F                  PIC X(1).

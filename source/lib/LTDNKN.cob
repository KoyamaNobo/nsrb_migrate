      *********************************************
      *****                                   *****
      *****       �`�[�������t�@�C���@�@�@    *****
      *****                       16/16       *****
      *********************************************
       01  JT-DNKN.
           02  JT-DNKN_PNAME1     PIC  X(007) VALUE "JT-DNKN".
           02  F                  PIC  X(001).
           02  JT-DNKN_LNAME      PIC  X(007) VALUE "JT-DNKN".
           02  F                  PIC  X(001).
           02  JT-DNKN_KEY1       PIC  X(100) VALUE SPACE.
           02  JT-DNKN_KEY2       PIC  X(100) VALUE SPACE.
           02  JT-DNKN_KEY3       PIC  X(100) VALUE SPACE.
           02  JT-DNKN_KEY4       PIC  X(100) VALUE SPACE.
           02  JT-DNKN_KEY5       PIC  X(100) VALUE SPACE.
           02  JT-DNKN_SORT       PIC  X(100) VALUE SPACE.
           02  JT-DNKN_IDLST      PIC  X(100) VALUE SPACE.
           02  JT-DNKN_RES        USAGE  POINTER.
       01  DNKN-R.
           02  DNKN-KEY.
             03  DNKN-01          PIC 9(01).                            �q�ɂb�c
             03  DNKN-02          PIC 9(06).                            �i���b�c
             03  DNKN-03          PIC X(01).                            ں��ދ�
             03  DNKN-04.
                 04  DNKN-041     PIC 9(06).                            �`�[���@
                 04  DNKN-042     PIC 9(01).                            �s���@�@
           02  F                  PIC X(01).
       77  F                      PIC X(01).

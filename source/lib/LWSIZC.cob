      *******************************************************************
      *    �T�C�Y�R�[�h�e�[�u��                                         *
      *******************************************************************
       01  SIZE-CD-TBL.
           02  F  PIC X(30) VALUE "001002010011012013014280290300".
           02  F  PIC X(30) VALUE "125130135140150160170180190200".
           02  F  PIC X(30) VALUE "210215220225230235240245250000".
           02  F  PIC X(30) VALUE "240245250255260265270275000000".
       01  F   REDEFINES SIZE-CD-TBL.
           02  SIZE-CD-T1     OCCURS  04.
               03  SIZE-CD    PIC  9(03)  OCCURS  10.
      *******************************************************************
      *    �T�C�Y���̃e�[�u��                                           *
      *******************************************************************
       01  SIZE-NM-TBL.
           02  F  PIC N(20) VALUE
               "�@�@�@�@�@�@�@�@�r�r�@�@�r�@�@�@�l�@�@�@".
           02  F  PIC N(20) VALUE
               "�k�@�@�@�k�k�@�@�w�k�@�@�w�w�k�@�R�O�D�O".
           02  F  PIC N(20) VALUE
               "�P�Q�D�T�P�R�D�O�P�R�D�T�P�S�D�O�P�T�D�O".
           02  F  PIC N(20) VALUE
               "�P�U�D�O�P�V�D�O�P�W�D�O�P�X�D�O�Q�O�D�O".
           02  F  PIC N(20) VALUE
               "�Q�P�D�O�Q�P�D�T�Q�Q�D�O�Q�Q�D�T�Q�R�D�O".
           02  F  PIC N(20) VALUE
               "�Q�R�D�T�Q�S�D�O�Q�S�D�T�Q�T�D�O�@�@�@�@".
           02  F  PIC N(20) VALUE
               "�Q�S�D�O�Q�S�D�T�Q�T�D�O�Q�T�D�T�Q�U�D�O".
           02  F  PIC N(20) VALUE
               "�Q�U�D�T�Q�V�D�O�Q�V�D�T�@�@�@�@�a��@�@".
           02  F  PIC N(20) VALUE
               "�R���@�@�Q���@�@�P���@�@�O���@�@���@�@�@".
           02  F  PIC N(40) VALUE
               "��@�@�@����@�@�Q�W�D�O�Q�X�D�O�R�O�D�O".
       01  F   REDEFINES SIZE-NM-TBL.
           02  SIZE-NM-T1     OCCURS  05.
               03  SIZE-NM    PIC  N(04)  OCCURS  10.
      *******************************************************************
      *    �T�C�Y���[�N                                                 *
      *******************************************************************
       01  SIZE-WK.
           02  SIZE-WK-HIN    PIC  9(06).                               �i������
           02  SIZE-WK-CD     PIC  9(03).                               ���޺���
           02  SIZE-WK-KB     PIC  9(01).                               ���ދ敪
           02  SIZE-WK-NM     PIC  N(04).                               ���ޖ���
           02  SIZE-WK-II     PIC  9(01).
           02  SIZE-WK-JJ     PIC  9(02).
           02  SIZE-WK-SW     PIC  9(01).

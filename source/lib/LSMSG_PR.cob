      ******************************************
      *    MESSEGE  AREA                       *
      *                                        *
      * WORKING-STORAGE SECTION  :  LSMSG_PR   *
      * PROCEDURE DIVISION       :  LSMSG_PR_P *
      ******************************************
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  01DISP-MSG-01    PIC X(50).
           02  DISP-MSG-SPACE.
               03  01DISP-MSG-SPACE PIC X(50).
           02  DISP-BUZ-B.
               03  FILLER           PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER           PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  01NOR-M01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "���@�}�X�^�@�o�^�ρ@��".
           02  NOR-D01.
               03  01NOR-D01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "���@�f�[�^�@�o�^�ρ@��".
           02  INV-M01.
               03  01INV-M01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "���@�}�X�^�@���o�^�@��".
           02  INV-D01.
               03  01INV-D01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "���@�f�[�^�@���o�^�@��".
           02  OK-01.
               03  01OK-01          PIC X(50).
      *                   COLOR   WHITE.
               03  FILLER           PIC N(07) VALUE
               "���@�n�@�j�@��".
           02  CAN-01.
               03  01CAN-01         PIC X(50).
      *                   COLOR   WHITE.
               03  FILLER           PIC N(09) VALUE
               "���@�L�����Z���@��".
           02  ERR-01.
               03  01ERR-01         PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(09) VALUE
               "���@���̓G���[�@��".
           02  INV-MCT.
               03  01INV-MCT        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(14) VALUE
                "���@�R���g���[���l���o�^�@��".
           02  INV-CON.
               03  01INV-CON        PIC X(50).
      *                   COLOR   YELLOW.
               03  FILLER           PIC N(21) VALUE
               "���@�R���g���[���e���o�^�@�������s�s�@��".
           02  ERR-YMD.
               03  01ERR-YMD        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "���@���t���̓G���[�@��".
      *******
           02  ERR-DIS.
               03  01ERR-DIS        PIC X(50).
      *                   COLOR   YELLOW.
               03  FILLER           PIC X(05) VALUE
               "<<<  ".
               03  03ERR-DIS        PIC X(12).
               03  04ERR-DIS        PIC X(01).
               03  FILLER           PIC X(11) VALUE
               "�װ STATUS=".
               03  06ERR-DIS        PIC X(02).
               03  FILLER           PIC X(05) VALUE
               "  >>>".
               03  FILLER           PIC X(05) VALUE
               " KEY=".
               03  FILLER           PIC X(30).
      *                   COLOR  YELLOW.
      *

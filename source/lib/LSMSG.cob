      *****************************************
      *    MESSEGE  AREA                      *
      *                                       *
      * WORKING-STORAGE SECTION  :  LSMSG     *
      * PROCEDURE DIVISION       :  LSMSG_P   *
      *****************************************
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  01DISP-MSG-01     PIC X(60).
           02  DISP-MSG-SPACE.
               03  01DISP-MSG-SPACE  PIC X(60).
           02  DISP-MSG-SPACES.
               03  01DISP-MSG-SPACES PIC X(40).
           02  DISP-BUZ-B.
               03  FILLER            PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER            PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER            PIC X(22) VALUE
                  "���@�}�X�^�@�o�^�ρ@��".
           02  NOR-D01.
               03  FILLER            PIC X(22) VALUE
                  "���@�f�\�^�@�o�^�ρ@��".
           02  INV-M01.
               03  FILLER            PIC X(22) VALUE
                  "���@�}�X�^�@���o�^�@��".
           02  INV-D01.
               03  FILLER            PIC X(22) VALUE
                  "���@�f�\�^�@���o�^�@��".
           02  OK-01.
               03  FILLER            PIC X(14) VALUE
                  "���@�n�@�j�@��".
           02  CAN-01.
               03  FILLER            PIC X(18) VALUE
                  "���@�L�����Z���@��".
           02  ERR-01.
               03  FILLER            PIC X(18) VALUE
                  "���@���̓G���\�@��".
           02  ERR-02.
               03  FILLER            PIC X(22) VALUE
                  "���@�f�[�^�@�Ȃ��@�@��".
           02  ERR-DIS.
               03  FILLER            PIC X(05) VALUE
               "<<<  ".
               03  02ERR-DIS         PIC X(12).
               03  03ERR-DIS         PIC X(01).
               03  FILLER            PIC X(11) VALUE
               "�װ STATUS=".
               03  05ERR-DIS         PIC X(02).
               03  FILLER            PIC X(05) VALUE
               "  >>>".
               03  FILLER            PIC X(05) VALUE
               " KEY=".
               03  08ERR-DIS         PIC X(30).
      **

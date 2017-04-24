      ****************************************************
      *         îrëºêßå‰ÉtÉ@ÉCÉãÅ@Å@IS   16/16           *
      ****************************************************
       01  JDCON.
           02  JDCON_PNAME1              PIC  X(005) VALUE "JDCON".
           02  F                         PIC  X(001).
           02  JDCON_LNAME               PIC  X(005) VALUE "JDCON".
           02  F                         PIC  X(001).
           02  JDCON_KEY1                PIC  X(100) VALUE SPACE.
           02  JDCON_KEY2                PIC  X(100) VALUE SPACE.
           02  JDCON_SORT                PIC  X(100) VALUE SPACE.
           02  JDCON_IDLST               PIC  X(100) VALUE SPACE.
           02  JDCON_RES       USAGE  POINTER.
      *
       01  JDCON-R.
           02   JDCON-KEY.
                03   JDCON-01            PIC 9(1).                      ID
                03   JDCON-02            PIC 9(06).                     ì`ï[áÇ
           02   FILLER                   PIC X(09).
       77  F                             PIC  X(001).

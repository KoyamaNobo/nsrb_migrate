      ****************************************
      * WORKING-STORAGE SECTION  :  LSSEM    *
      * PROCEDURE DIVISION       :  LSSEM_P  *
      ****************************************
       01  SCR-STN-ERR-MSG.
           02  E-STAT  PIC  X(002).
           02  E-ME71.
             03  01E-ME71  PIC  X(013) .
             03  FILLER    PIC  N(025) VALUE
                 "オーバーフロー、領域を拡張後、「ＣＴＲＬ＋ｆ・５」".
           02  E-ME78  PIC  N(002) VALUE "連絡".
           02  E-ME98  PIC  X(005) VALUE X"1B4A05".
           02  E-ME99  PIC  X(005) VALUE X"1B4205".
           02  E-CL.
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".

      ****************************************
      *****     カレンダー　ファイル     *****
      *****       ( CALNF )  16/16       *****
      ****************************************
       01  CALNM.
           02  CALNM_PNAME1   PIC  X(005) VALUE "CALNF".
           02  F              PIC  X(001).
           02  CALNM_LNAME    PIC  X(005) VALUE "CALNM".
           02  F              PIC  X(001).
           02  CALNM_KEY1     PIC  X(100) VALUE SPACE.
           02  CALNM_KEY2     PIC  X(100) VALUE SPACE.
           02  CALNM_SORT     PIC  X(100) VALUE SPACE.
           02  CALNM_IDLST    PIC  X(100) VALUE SPACE.
           02  CALNM_RES      USAGE  POINTER.
      *
       01  CALN-R.
           02  CL-KEY.                                                  KEY
             03  CL-NG.
               04  CL-NEN     PIC  9(004).                              ﾈﾝ
               04  CL-GET     PIC  9(002).                              ﾂｷ
             03  CL-PEY       PIC  9(002).                              ﾋ
           02  CL-KEYD  REDEFINES CL-KEY.
             03  CL-NEND      PIC  9(004).                              ﾈﾝ
             03  CL-GP.
               04  CL-GETD    PIC  9(002).                              ﾂｷ
               04  CL-PEYD    PIC  9(002).                              ﾋ
           02  CL-NGP   REDEFINES  CL-KEY.
             03  F            PIC  9(002).
             03  CL-NGPS      PIC  9(006).
           02  CL-DATE  REDEFINES  CL-KEY  PIC  9(008).
           02  CL-YB          PIC  9(001).                              ﾖｳﾋﾞ
           02  F              PIC  9(001).
           02  CL-SJ          PIC  9(001).                              BKｷｭｳｼﾞﾂ
           02  CL-AHO.
             03  CL-HOD   OCCURS   5.
               04  CL-H       PIC  9(001).
       77  F                  PIC X(1).

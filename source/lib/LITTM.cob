      ***********************************************
      *****        得意先 統計 マスター         *****
      *****         (  T T M  )  170/3          *****
      ***********************************************
       01  TT-M.
           02  TT-M_PNAME1    PIC  X(003) VALUE "TTM".
           02  F              PIC  X(001).
           02  TT-M_LNAME     PIC  X(004) VALUE "TT-M".
           02  F              PIC  X(001).
           02  TT-M_KEY1      PIC  X(100) VALUE SPACE.
           02  TT-M_SORT      PIC  X(100) VALUE SPACE.
           02  TT-M_IDLST     PIC  X(100) VALUE SPACE.
           02  TT-M_RES       USAGE  POINTER.
      *
       01  TT-R.
           02  TT-KEY.                                                  ｺｰﾄﾞ
             03  TT-TCD       PIC  9(004).                              得意先CD
      *    * * *   統　計　項　目　 * * *
           02  TT-TD.
             03  TT-TZZ       PIC S9(009).                              前月残高
             03  TT-TZZZ      PIC S9(007).                              　消費税
             03  TT-TUZ       PIC S9(009).                              売掛残高
             03  TT-TUZZ      PIC S9(007).                              　消費税
             03  TT-TUA       PIC S9(009).                              売上金額
             03  TT-TUAZ      PIC S9(007).                              　消費税
             03  TT-TNB       PIC S9(008).                              値引金額
             03  TT-TNBZ      PIC S9(006).                              　消費税
             03  TT-TNK       PIC S9(009).                              入金金額
             03  TT-TNKZ      PIC S9(007).                              　消費税
             03  TT-TUG       PIC S9(009).                              売上原価
      *    * * *   区分　コード　 * * *
           02  F              PIC  9(002).
           02  TT-TNC         PIC  9(002).                              担当者
           02  TT-FKC         PIC  9(002).                              都道府県
           02  TT-BC          PIC  9(001).                              部門ｺｰﾄﾞ
           02  TT-DCC         PIC  9(001).
           02  TT-YUG         PIC S9(009).
           02  F              PIC  X(020).
       77  F                  PIC  X(001).

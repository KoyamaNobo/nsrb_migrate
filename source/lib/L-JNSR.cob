      **************************************
      *****     入出庫累積ファイル     *****
      **************************************
       01  JNSR.
           02  JNSR_PNAME1            PIC  X(005) VALUE "JNSR1".
           02  F                      PIC  X(001).
           02  JNSR_PNAME2            PIC  X(005) VALUE "JNSR2".
           02  F                      PIC  X(001).
           02  JNSR_PNAME3            PIC  X(005) VALUE "JNSR3".
           02  F                      PIC  X(001).
           02  JNSR_LNAME             PIC  X(004) VALUE "JNSR".
           02  F                      PIC  X(001).
           02  JNSR_KEY1              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY2              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY3              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY4              PIC  X(100) VALUE SPACE.
           02  JNSR_KEY5              PIC  X(100) VALUE SPACE.
           02  JNSR_SORT              PIC  X(100) VALUE SPACE.
           02  JNSR_IDLST             PIC  X(100) VALUE SPACE.
           02  JNSR_RES               USAGE  POINTER.
       01  JNSR-R.
           02  JNSR-KEY1.                                               KEY1
             03  JNSR-01              PIC  9(06)  COMP-3.               品名ｺｰﾄﾞ
             03  JNSR-02              PIC  9(08)  COMP-3.
             03  JNSR-03              PIC  9(02).                       入出力区
             03  JNSR-04              PIC  9(06)  COMP-3.               伝票№
             03  JNSR-05              PIC  9(01).                       行
           02  JNSR-06                PIC  9(01).                       倉ｺｰﾄﾞ
           02  JNSR-07                PIC  9(01).                       サイズ
           02  JNSR-08.                                                 入出庫数
             03  JNSR-081             PIC S9(04)  COMP-3    OCCURS  10.
           02  JNSR-09                PIC  9(01).                       生産区分
           02  JNSR-10                PIC  9(01).                       出荷伝区
           02  JNSR-11.
             03  JNSR-111             PIC  9(04).                       得意先C
             03  JNSR-112             PIC  9(03).                       直送NO
           02  JNSR-12                PIC  9(06)  COMP-3.               送り状№
           02  JNSR-13                PIC  9(01).                       預り区分
           02  JNSR-14                PIC  9(01).                       運送C
           02  JNSR-KEY2.
             03  JNSR-15.                                               受注
               04  JNSR-151           PIC  9(06)  COMP-3.               　受注№
               04  JNSR-152           PIC  9(01).                       　行№
             03  JNSR-16              PIC  9(08)  COMP-3.
             03  JNSR-17              PIC  9(01).                       ﾚｺｰﾄﾞKBN
             03  JNSR-18.                                               伝票
               04  JNSR-181           PIC  9(06)  COMP-3.               　伝票№
               04  JNSR-182           PIC  9(01).                       　行№
           02  JNSR-KEY3.
             03  JNSR-19              PIC  9(04).                       得意先C
             03  JNSR-20              PIC  9(08)  COMP-3.
             03  JNSR-21              PIC  9(01).                       レコー区
             03  JNSR-22.                                               伝票№
               04  JNSR-221           PIC  9(06)  COMP-3.               出荷指№
               04  JNSR-222           PIC  9(01).                       　行№
           02  JNSR-23                PIC  N(09).                       配達
           02  JNSR-24                PIC  N(23).                       摘要
           02  JNSR-81                PIC  9(08)  COMP-3.
           02  JNSR-82                PIC  9(03).
           02  JNSR-90                PIC  9(01).                       入力部署
           02  JNSR-91                PIC  9(01).                       繰越区分
           02  JNSR-92                PIC  9(02).                       処理月
       77  F                          PIC  X(001).

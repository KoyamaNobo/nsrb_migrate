000010*    ﾀｲｼｮｸｷｭｳﾖﾜｰｸ       *
000020 FD  NT-TAW
000030     BLOCK      CONTAINS     8      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "NT-TAW".
000060 01  TAW-R.
000070     02  TAW-KEY.                                                 KEY
000080       03  TAW-01        PIC X(04).                               社員CD
000090     02  TAW-02.                                                  年令
000100       03  TAW-021       PIC 9(02).                               年
000110       03  TAW-022       PIC 9(02).                               月
000120     02  TAW-03.                                                  勤続年数
000130       03  TAW-031       PIC 9(02).                               年
000140       03  TAW-032       PIC 9(02).                               月
000150     02  TAW-04          PIC 9(02)V9(03).                         支給率
000160     02  TAW-05          PIC 9(10).                               金額
000170     02  TAW-06          PIC 9(03).                               年率
000180     02  TAW-07          PIC 9(01).                               性別
000190     02  FILLER          PIC X(01).

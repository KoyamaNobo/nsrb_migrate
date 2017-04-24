000010*    ﾋｿﾞｹﾌｧｲﾙ           *
000020 FD  NT-HIF
000030     BLOCK      CONTAINS     1      RECORDS
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "NT-HIF".
000060 01  HIF-R.
000070     02  HIF-KEY.                                                 KEY
000080       03  HIF-01        PIC X(02).                               ﾚｺｰﾄﾞKBN
000090     02  HIF-02.                                                  期末
000100       03  HIF-021       PIC 9(04).                               年
000110       03  HIF-022       PIC 9(02).                               月
000120     02  HIF-03.                                                  作表更新
000130       03  HIF-031       PIC 9(04).                               年
000140       03  HIF-032       PIC 9(02).                               月
000150     02  HIF-04.                                                  西暦ｺﾝﾄﾛ
000160       03  HIF-041       OCCURS  2.
000170         04  HIF-0411    PIC 9(02).                               FROM年
000180         04  HIF-0412    PIC 9(02).                               TO年
000190         04  HIF-0413    PIC 9(04).                               西暦
000200     02  HIF-05.                                                  支給率打
000210       03  HIF-051       PIC 9(02).                               男
000220       03  HIF-052       PIC 9(02).                               女
000230     02  FILLER          PIC X(222).

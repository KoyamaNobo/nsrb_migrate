SCREEN SCHM21                                                              00010
ARE (01,24)_                                                               00020
CON (01,16) "＊＊＊　　直送先マスター　メンテナンス　　＊＊＊"_            00030
CON (03,18) "登録=1 修正=2 削除=3 リスト=4 終了=9   ﾘﾀｰﾝ"_                 00040
BOX (05,04) (06,76)_ UND (05,04) (,76)_                                    00050
VER (05,14) (06)_                                                          00060
CON (05,05) "ｺｰﾄﾞ      得意先名"_                                          00070
CON (06,05) "ｺｰﾄﾞ      直送先名"_                                          00080
VER (07,66)_  VER (07,73)_  VER (07,76)_                                   00090
UND (07,66) (,76)_                                                         00100
CON (07,67) NC"作表��"_                                                    00110
BOX (08,04) (17,56)_                                                       00120
OVE (14,56) (,67)_       UND (14,56) (,67)_                                00130
VER (14,65)_             VER (14,67)_                                      00140
CON (08,05) "都道府県��"_                                                  00150
CON (09,05) "住所 (上)"_ CON (10,10) "(下)"_                               00160
CON (11,05) "郵便番号"_                                                    00170
CON (12,05) "電話番号"_                                                    00180
CON (13,05) "運送ｺｰﾄﾞ"_                                                    00190
CON (14,05) "売上区分"_                                                    00200
CON (14,20) "（0=しない､5=する）"_                                         00210
CON (15,05) "在中区分"_                                                    00220
CON (15,20) "（0="_                                                        00230
CON (15,24) NC"発送明細なし"_                                              00240
CON (15,36) "､1="_                                                         00250
CON (15,39) NC"あり）"_                                                    00260
CON (16,05) "ナ フ コ"_                                                    00270
CON (16,21) NC"着店日数"_                                                  00280
CON (17,05) "納入区分"_                                                    00290
CON (17,21) "(0=店入れ,1=直送)"_                                           00300
CON (23,50) "確認 OK=1 NO=9   ﾘﾀｰﾝ"_                                       00310
END_                                                                       00320

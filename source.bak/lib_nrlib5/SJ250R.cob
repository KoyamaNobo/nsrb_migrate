SCREEN      SJ250R                                                         00010
MODE        K25                                                            00020
ARE (01,24)_                                                               00030
REV (01,01)(,12)_                                                          00040
UND (01,01)(,80)_  UND (02,01)(,80)_  UND (03,01)(,80)_  UND (13,01)(,80)_ 00050
UND (23,01)(,80)_  UND (24,40)(,80)_                                       00060
VER (02,61)_       VER (03,40)(24)_                                        00070
**                                                                         00080
CON (01,02) NC"在庫問合せ"_                                                00090
CON (01,21)   "(当日処理分を含む=0,引当情報を含む=1,終了=PF9)-->( )"_      00100
CON (02,01)   "品名:"_    CON (02,62)   "倉庫:"_                           00110
CON (03,01) "ｻｲｽﾞ 前日残"_    CON (03,41) "ｻｲｽﾞ 前日残"_                   00120
CON (04,01) "12.5"_           CON (04,41) "24.0"_                          00130
CON (05,01) "13.0"_           CON (05,41) "24.5"_                          00140
CON (06,01) "13.5"_           CON (06,41) "25.0"_                          00150
CON (07,01) "14.0"_           CON (07,41) "25.5"_                          00160
CON (08,01) "15.0"_           CON (08,41) "26.0"_                          00170
CON (09,01) "16.0"_           CON (09,41) "26.5"_                          00180
CON (10,01) "17.0"_           CON (10,41) "27.0"_                          00190
CON (11,01) "18.0"_           CON (11,41) "27.5"_                          00200
CON (12,01) "19.0"_           CON (12,41) "----"_                          00210
CON (13,01) "20.0"_           CON (13,41) "預り"_                          00220
CON (14,01) "21.0"_           CON (14,41) "３号"_                          00230
CON (15,01) "21.5"_           CON (15,41) "２号"_                          00240
CON (16,01) "22.0"_           CON (16,41) "１号"_                          00250
CON (17,01) "22.5"_           CON (17,41) "０号"_                          00260
CON (18,01) "23.0"_           CON (18,41) " 中 "_                          00270
CON (19,01) "23.5"_           CON (19,41) " 大 "_                          00280
CON (20,01) "24.0"_           CON (20,41) " 特 "_                          00290
CON (21,01) "24.5"_           CON (21,41) "28.0"_                          00300
CON (22,01) "25.0"_           CON (22,41) "29.0"_                          00310
CON (23,01) "----"_           CON (23,41) "30.0"_                          00320
                              CON (24,41) "(計)"_                          00330
END_                                                                       00340

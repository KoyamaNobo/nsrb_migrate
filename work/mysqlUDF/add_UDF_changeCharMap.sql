#辞書順と数値順は固定長の時同じ
#DELIMITERを変え手元に戻さないといけない？？
DROP FUNCTION IF EXISTS chCharMap;
CREATE FUNCTION chCharMap returns string soname 'changeCharMap.so';

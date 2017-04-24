SELECT DISTINCT      SUBSTRING_INDEX(x.TableName, '_', 1) AS TN
,levenshtein(SUBSTRING_INDEX(REPLACE(x.TableName,'-',''), '_', 1),:TableName) as lev
,x.Label      ,x.S_Point      ,x.Size      ,x.DataType      ,y.Japanese_Name
FROM      testdb.M_ITEMELEMENT AS x
LEFT JOIN dataview.m_japanese_name AS y
ON              SUBSTRING_INDEX(x.TableName, '_', 1) = y.TableName
AND x.Label     = y.Label
AND x.S_Point   = y.S_Point
AND x.Size      = y.Size
AND x.DataType  = y.DataType
AND y.Del_Flg   = 0
WHERE      (y.NonDisp_Flg != '1' OR y.NonDisp_Flg is NULL)
ORDER BY      lev ASC ,     TN
,     x.S_Point
,     x.Size  LIMIT 0 , 60 ;

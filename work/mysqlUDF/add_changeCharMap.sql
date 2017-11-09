#辞書順と数値順は固定長の時同じ
#DELIMITERを変え手元に戻さないといけない？？
DROP FUNCTION IF EXISTS chCharMap;
DELIMITER //
CREATE FUNCTION chCharMap(f VARCHAR(2048)) RETURNS VARCHAR(2048) DETERMINISTIC
BEGIN
    DECLARE _result VARCHAR(2048);
    DECLARE _length INT;
    DECLARE _counter INT;
    
    set _counter=0;
    set _result='';
    set _length=length(f);
    
    while _counter < _length do
        if substr(HEX(f),((_counter * 2) +1),1) = 3 then
            if substr(HEX(f),((_counter * 2) +2),1) <= 9 then
                #数字の時
                set _result = CONCAT(_result,UNHEX(CONCAT('F',substr(HEX(f),((_counter * 2) +2),1))));
            end if;
        elseif substr(HEX(f),((_counter * 2) +1),1) = 6 OR substr(HEX(f),((_counter * 2) +1),1) = 7 then
            #小文字の時
            set _result = CONCAT(_result,UNHEX(substr(HEX(f),((_counter * 2) + 1),2) + 30 ));
        elseif substr(HEX(f),((_counter * 2) +1),1) = 4 OR substr(HEX(f),((_counter * 2) +1),1) = 5 then
            if substr(HEX(f),((_counter * 2) +1),1) = 4 then
                #大文字の前半
                set _result = CONCAT(_result,UNHEX(CONCAT('C',substr(HEX(f),((_counter * 2) +2),1))));
            elseif substr(HEX(f),((_counter * 2) +1),1) = 5 then
                #大文字の後半
                set _result = CONCAT(_result,UNHEX(CONCAT('D',substr(HEX(f),((_counter * 2) +2),1))));
            end if;
        else
            set _result = CONCAT(_result,UNHEX(substr(HEX(f),((_counter * 2) +1),2)));
        end if;
        set _counter = _counter + 1;
    end while;
    
    RETURN (_result);
END;
//
DELIMITER ;

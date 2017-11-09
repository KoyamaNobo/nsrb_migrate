#�������Ɛ��l���͌Œ蒷�̎�����
#DELIMITER��ς��茳�ɖ߂��Ȃ��Ƃ����Ȃ��H�H
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
                #�����̎�
                set _result = CONCAT(_result,UNHEX(CONCAT('F',substr(HEX(f),((_counter * 2) +2),1))));
            end if;
        elseif substr(HEX(f),((_counter * 2) +1),1) = 6 OR substr(HEX(f),((_counter * 2) +1),1) = 7 then
            #�������̎�
            set _result = CONCAT(_result,UNHEX(substr(HEX(f),((_counter * 2) + 1),2) + 30 ));
        elseif substr(HEX(f),((_counter * 2) +1),1) = 4 OR substr(HEX(f),((_counter * 2) +1),1) = 5 then
            if substr(HEX(f),((_counter * 2) +1),1) = 4 then
                #�啶���̑O��
                set _result = CONCAT(_result,UNHEX(CONCAT('C',substr(HEX(f),((_counter * 2) +2),1))));
            elseif substr(HEX(f),((_counter * 2) +1),1) = 5 then
                #�啶���̌㔼
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

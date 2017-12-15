#Check_Returns_Cobol
set _JRCODE_TXT = "";
set _ERROR_TXT = "";
#JRCODEの変更
if (-e "$MAP_JRCODE_FILE") then
	foreach _JRCODE_TXT (`cat $MAP_JRCODE_FILE`)
		set JRCODE = $_JRCODE_TXT;
	end
	cat /dev/null > $MAP_JRCODE_FILE
endif
#ABORT,NORMALの変更
if (-e "$MAP_ERROR_FILE") then
	#初期値を設定しなおす必要がある
	set ABORT = 0;
	set NORMAL = 1;
	foreach _ERROR_TXT ("`cat $MAP_ERROR_FILE`")
		if(`echo "$_ERROR_TXT" |wc -c` < 2) then
			set ABORT = 0;
			set NORMAL = 1;
		else
			set ABORT = 1;
			set NORMAL = 0;
			echo " [ERR] $_ERROR_TXT";
			logger -i `echo ":${0}: $_ERROR_TXT " | awk '{gsub("\\[", " ");print $0;}'| awk '{gsub("\\]", " ");print $0;}'`;
		endif
	end
	cat /dev/null > $MAP_ERROR_FILE
endif

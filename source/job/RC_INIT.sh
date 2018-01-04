#Run_Cobol_initialize
if (${#argv} >= 1) then
	set USER_ID = ${argv[1]};
else
	set USER_ID = STN000;
endif
set JRCODE=000;
set FILENAME_SUFFIX = `head /dev/urandom | base64 | fold -w 8 |tail -n +1 | head -1`;
set MAP_JRCODE_FILE = ${JRCODE_PATH}${USER_ID}$FILENAME_SUFFIX;
setenv MAP_JRCODE_FILE $MAP_JRCODE_FILE;
if ($?ERROR_EXEC_PATH) then
	set MAP_ERROR_FILE = ${ERROR_EXEC_PATH};
else
	set MAP_ERROR_FILE = ${ERROR_PATH}${USER_ID}$FILENAME_SUFFIX;
endif
logger -i `echo ":start:${0}: $MAP_JRCODE_FILE $MAP_ERROR_FILE " `;

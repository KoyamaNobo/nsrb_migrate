#!/bin/sh
cdir=`pwd`
targdir='/usr/local/bin'
if [ $# -eq 1 ]; then
echo ${1}
    targdir=${1}
fi

gcc -g -o ${targdir}/OVERRAY ${cdir}/creOverlay.c -lhpdf `xml2-config --cflags --libs` `mariadb_config --include --libs` `cob-config --libs` -I /usr/local/lib/map 

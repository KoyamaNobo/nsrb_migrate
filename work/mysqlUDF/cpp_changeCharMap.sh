#!/bin/sh
if [ $# -gt 0 ];then
  echo "debug $#"
  #gcc -shared -fPIC -v -g -o/usr/lib64/mysql/plugin/changeCharMap.so ./changeCharMap.cpp `mariadb_config --libs --include` 
  gcc -shared -fPIC -v -g -o/usr/lib/mysql/plugin/changeCharMap.so ./changeCharMap.cpp `mariadb_config --libs --include` 
else
  echo "release $#"
  gcc -shared -fPIC -v -o./changeCharMap.so ./changeCharMap.cpp `mariadb_config --libs --include` 
  #gcc -g -o./changeCharMap ./changeCharMap.cpp `mariadb_config --libs` 
fi

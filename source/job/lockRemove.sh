#!/bin/sh
#DBNAME=nisshinMap
DBNAME=testdb
USER=mysql
PASSWORD=mysql
#HOST=192.168.1.254
HOST=127.0.0.1

if [ $# -ne 1 ];then
    mysql --user=$USER --password=$PASSWORD --host=$HOST $DBNAME -e "SELECT * FROM M_TBLMANAGE WHERE LockMode <> '' AND LockMode IS NOT NULL;"
    echo "Are you sure you want to delete this record ? (yes/no)"
    read agree
    if [ "$agree" = "yes" ];then
        mysql --user=$USER --password=$PASSWORD --host=$HOST $DBNAME -e 'UPDATE M_TBLMANAGE SET LockMode="";COMMIT;'
        echo "It has been deleted"
    else
        echo "It did not delete"
    fi
else
    mysql --user=$USER --password=$PASSWORD --host=$HOST $DBNAME -e "SELECT * FROM M_TBLMANAGE WHERE TableName Like '${1}';"
    echo "Are you sure you want to delete this record ? (yes/no)"
    read agree
    if [ "$agree" = "yes" ];then
        mysql --user=mysql --password=$PASSWORD $DBNAME -e "UPDATE M_TBLMANAGE SET Uid=NULL,LockMode='' WHERE TableName Like '${1}';COMMIT;"
        echo "It has been deleted"
    else
        echo "It did not delete"
    fi
fi

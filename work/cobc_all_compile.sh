#!/bin/dash
cd ../source/exec/
num=0
if [ $# -ge 1 ];then
    num=$# 
fi
ls -al | grep .cob |awk '{if($9~/^[0-9]*$/){ print $10 }else{print $9}}' | while read LINE; do
    echo "processing $LINE"
    fname=`echo "$LINE"  | awk -F'[.]' '{print $1}'`
    if [ $num -ne 0 ];then
        if [ $num -ne 1 ];then
            echo "_de"
            my_cobc_de $fname
        else
            echo "_bak"
            my_cobc_bak $fname
        fi
    else
        mycobc $fname
    fi
#    echo $fname
done

#!/bin/sh
ls -al | grep .txt | awk '{ print $9 }' | while read LINE; do

fname=`echo "$LINE" | awk -F'[.]' '{print $1}'|tr [:lower:] [:upper:]`
        cobc -I ../lib_nrlib5 $LINE
done
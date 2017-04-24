#/bin/sh
ls -al | grep .txt | awk '{ print $10 }' | while read LINE; do


fname=`echo "$LINE" | awk -F'[.]' '{print $1}'`
        mv "$LINE" "$fname".sh
done

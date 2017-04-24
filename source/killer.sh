ps aux|grep php | awk '{ print $2 }' |while read LINE; do
kill -TERM $LINE
done
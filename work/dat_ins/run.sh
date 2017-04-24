# run.sh
run() {
  "$@"
  result=$?
  if [ $result -ne 0 ]
  then
    echo "Failed: $@ [$PWD]"  
    exit $result
  fi
  return 0
}
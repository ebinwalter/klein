#!/bin/sh
shopt -s nullglob

pushd $(dirname "$0")

rm -rf test-out
mkdir test-out

if [[ $makerefs -eq 1 ]]; then
  rm -rf expected
  mkdir expected
fi

for i in *.kl; do
  ../target/debug/kleinc $i > /dev/null
  stub="${i%.kl}"
  if [[ $makerefs -eq 1 ]]; then
    if [ -f $stub.in ]; then
      cat $stub.in | java -jar ../Mars4_5.jar sm $stub.s > expected/$stub.txt
    else 
      java -jar ../Mars4_5.jar sm "$stub".s > expected/$stub.txt
    fi
  else
    if [ -f $stub.in ]; then
      cat $stub.in | java -jar ../Mars4_5.jar sm $stub.s > test-out/$stub.txt
    else 
      java -jar ../Mars4_5.jar sm "$stub".s > test-out/$stub.txt
    fi
    diff=$(diff -s test-out/$stub.txt expected/$stub.txt)
    if [[ "$diff" =~ .*identical.* ]]; then
      echo "Passed test"
    else
      echo "Failed test"
    fi 
  fi
done

popd

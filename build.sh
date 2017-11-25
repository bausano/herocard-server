#!/bin/bash

for filename in **/*.erl; do
  cp $filename bin/$(basename $filename)
done

cd bin

echo -e "\nCompiling\n"

erl -make

for filename in *.erl; do
  rm $filename
done

echo -e "\nRunning\n"

erl -s main start -s init stop

echo -e "\n\nTerminating\n"

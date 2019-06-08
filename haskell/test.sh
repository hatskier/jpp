#!/bin/bash

if [[ "$1" == "clean" ]]; then
  rm -f *.out
  exit 0
fi

if [[ ! -z "$1" ]]; then
  files="good/good$1.in"
else
  files="good/good*.in"
fi

for infile in $(ls $files); do
  echo "------------TEST $infile------------"
  outfile=${infile/%.in/.out}
  ansfile=${infile/%.in/.ps}
  ./Main <"$infile" >"$outfile"
  diff "$outfile" "$ansfile"
done
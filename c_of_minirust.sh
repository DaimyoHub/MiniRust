#!/bin/sh

if [ ! -z "$1" -a "$1" != " " ]; then
  output_file=$(basename $1)
  output_file=${output_file%.*}

  C=1 dune exec minirust/minirust.exe $1 > $output_file.c

  if [ ! -s $output_file.c ]; then
    rm -f $output_file.c
  fi
fi


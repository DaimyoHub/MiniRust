#!/bin/sh

cat ./tests/output | awk \
  -v success=0 -v fail=0 -v last=1 -v completness=0 -v soundness=0 -v total=0 \
  'BEGIN { }

   $1 ~ /Running/      { if (last == 0) { success += 1; last = 0 } else { last = 0 }; total += 1 }
   $1 ~ /Test/         { fail += 1; last = 1 }

   $0 ~ /not ret/ {
     if (match($2, /[0-9]+/)) {
       incomplete_tests[completness] = substr($2, RSTART, RLENGTH)
       completness += 1
     }
   }

   $0 ~ /should ret/   { soundness += 1 }

   END {
     printf "Tested %d instances :\n", total
     printf "\t- %d tests successfully passed.\n", success
     printf "\t- %d tests failed:\n", fail
     printf "\t\t- %d do not raise an error but should.\n", soundness
     printf "\t\t- %d do raise an error but should not", completness

     if (completness > 0) {
       printf " ("
       for (i = 0; i < completness; i++) {
         printf "%s", incomplete_tests[i]
         if (i < completness - 1) {
           printf ", "
         }
       }
       printf ")"
     }

     printf ".\n"
   }'

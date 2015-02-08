./LazyEval.exe $1 $2 +RTS -p -hc -xt -i0.1
mv LazyEval.hp LazyEval_$1_$2.hp
mv LazyEval.prof LazyEval_$1_$2.prof
hp2ps LazyEval_$1_$2.hp

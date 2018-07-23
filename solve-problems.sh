make -f makefile all
for f in ./problems/FA/*.mdl; do
    echo "$f"
    #./main -t "$f"
done
for f in ./problems/FD/*.mdl; do
    echo "$f"
    #./main -s "$f"
done
for f in ./problems/FR/*_src.mdl; do
    dir=${f%/*}
    base=${f##*/}
    y=${dir}/${base%_src.mdl}_tgt.mdl
    echo "$f"
    echo "$y"
    #./main -s "$f" -t "$y"
done


make -f makefile all
for f in ./problems/*.mdl; do
    ./main -f "$f"
done

